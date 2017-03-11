{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- This module implements a simple yet efficient algorithm for automatically
-- testing and merging git branches. Here is a sketch of the idea,
--
-- Say we have a branch (call it @master@), which the following history,
-- @
-- ... ── A ── B ⇐ master
-- @
-- The algorithm begins with the following state,
--
--  * a queue of /merge requests/ currently being tested or blocked, @requests = []@
--  * a commit hash, @branch_head = B@, the current state of @master@
--  * a commit hash, @future_head = B@, which will point to the future state of
--    the branch, assuming all of the requests in @requests@ pass.
--
-- When we receive a request to merge a branch, call it @feature-1@,
-- @
-- .. ── A ── B  ⇐ master
--       ╰── C   ⇐ feature-1
-- @
-- we will first rebase @feature-1@ on top of @branch_head@, producing rebased
-- branch @feature-1'@,
-- @
-- .. ── A ── B  ⇐ master
--            ╰── C'  ⇐ feature-1'
-- @
-- We will now update @future_head := C'@, add an entry for this request it to
-- @requests@, and fire off a CI job to build and test @feature-1'@. When
-- @merge-1@\'s CI job passes, we will merge it to @master@. From this point
-- forth we will handle future merge requests under the optimistic assumption
-- that this build will succeed and consequently that @feature-1'@ will be merge
-- to @master@.
--
-- Now say we receive another request, this time to merge @feature-2@,
-- @
-- .. ── A ── B       ⇐ master
--        ╰── D ── E  ⇐ feature-2
-- @
-- Again we will rebase @feature-2@ on top of @branch_head@ (now @C'@), producing
-- a branch headed by @E'@,
-- @
-- .. ── A ── B                 ⇐ master
--            ╰── C'            ⇐ feature-1'
--                ╰── D' ── E'  ⇐ feature-2'
-- @
-- We will then update @future_head := E'@, add an entry to @requests@, and fire
-- off another CI job for @feature-2'@. Since @merge-2@ chronologically
-- succeeded @merge-1@, we say that @merge-2@ /depends on/ @merge-1@.
-- @feature-2'@ will be merged to @master@ when it's CI job and that of @merge-1@ passes.
-- If @merge-2@\'s CI job passes before we hear back about @merge-1@, we will
-- block before proceeding.
--
-- If @merge-1@ fails, we will pop its entry off of the head of @requests@,
-- cancel @merge-2@'s build if it hasn't already finished, and rebase @merge-2@
-- on top of @branch_head@. Likewise we would do the same for any of its
-- dependents.
--
module PushMerge where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Trans.State
import Control.Monad.Catch
import Data.Maybe
import Data.Semigroup
import Data.Foldable (toList, asum)
import GHC.Generics
import System.Process

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Sequence as Seq

import Servant (ToHttpApiData, FromHttpApiData)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import Control.Lens

import RpcChannel
import Git
import Utils

theRepo = GitRepo "."

runBranch :: ManagedBranch -> IO ()
runBranch managedBranch = do
    return ()

isManagedBranch :: Ref -> Maybe ManagedBranch
isManagedBranch = Just . ManagedBranch

isMergeBranch :: Ref -> Maybe Ref
isMergeBranch (Ref ref) = Ref <$> T.stripPrefix "refs/heads/merge/" ref

startServer :: GitRepo -> IO Server
startServer serverRepo = do
    serverBranches <- newTVarIO mempty
    serverNextRequestId <- newTVarIO (MergeRequestId 0)
    let serverStartBuild _ _ = return BuildSucceeded -- TODO
    return $ Server {..}

data NewMergeRequestError = BranchNotManaged
                          | CommitHasNoMergeBase
                          deriving (Show, Generic)
                          deriving anyclass (FromJSON, ToJSON)

newMergeRequest :: Server -> Ref -> SHA
                -> IO (Either NewMergeRequestError MergeRequestId)
newMergeRequest server ref headSha
  | Just managed <- isManagedBranch ref = undefined -- TODO
  | otherwise = return $ Left BranchNotManaged

freshRequestId :: Server -> IO MergeRequestId
freshRequestId server = atomically $ do
    i <- readTVar $ serverNextRequestId server
    writeTVar (serverNextRequestId server) $ case i of MergeRequestId i -> MergeRequestId (i+1)
    return i

withWorkingDir :: Server -> (GitRepo -> IO a) -> IO a
withWorkingDir = undefined -- TODO

data Server = Server { serverBranches      :: TVar (M.Map ManagedBranch (TQueue SomeBranchRequest))
                     , serverNextRequestId :: TVar MergeRequestId
                     , serverStartBuild    :: ManagedBranch -> BuildAction
                     , serverRepo          :: GitRepo
                     }

-- | A branch which we are responsible for merging into.
newtype ManagedBranch = ManagedBranch { getManagedBranch :: Ref }
                      deriving (Show, Eq, Ord, Generic)
                      deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype MergeRequestId = MergeRequestId Int
                       deriving (Eq, Ord, Show, Generic)
                       deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

-- | A request to merge some commits.
data MergeRequestState
    = MergeRequestState { _mergeRequestId   :: MergeRequestId
                        , _mergeOrigCommits :: CommitRange
                        , _mergeStatus      :: RequestStatus (Async BuildResult)
                        , _mergeBranch      :: ManagedBranch
                        }

-- | The status of a merge request.
data RequestStatus a
    = PendingBuild                     -- ^ needs a build to be started
    | FailedToRebase SHA               -- ^ failed to rebase onto the given base commit
    | Building CommitRange a           -- ^ building the given rebased commits
    | FailedToBuild CommitRange String -- ^ failed to build the given rebased commits
    | Succeeded CommitRange            -- ^ the given rebased commits were built sucessfully
    deriving (Show, Functor)

data BuildResult = BuildSucceeded
                 | BuildFailed String
                 deriving (Generic)
                 deriving anyclass (FromJSON, ToJSON)

-- | Test-build a SHA, returning 'Just' on error or 'Nothing' on success.
type BuildAction = SHA -> IO BuildResult





data SomeBranchRequest where
    SomeBranchRequest :: BranchRequest a -> SomeBranchRequest

data BranchRequest a where
    NewMergeRequest :: { newMergeReqCommits :: CommitRange
                       , newMergeReqBranch  :: ManagedBranch
                       }
                    -> BranchRequest MergeRequestId
    CancelMergeRequest :: { cancelMergeReqId :: MergeRequestId }
                       -> BranchRequest ()
    GetBranchStatus :: BranchRequest BranchStatus

data BranchStatus = BranchStatus { branchCurrentHead :: SHA
                                 , branchMergeRequests :: [(MergeRequestId, RequestStatus ())]
                                 }

--------------------------------------------------
-- Queue
--------------------------------------------------

newtype Queue a = Queue (Seq.Seq a)
                deriving (Show, Functor, Foldable)

popQueue :: Queue a -> Maybe (a, Queue a)
popQueue (Queue Seq.Empty)      = Nothing
popQueue (Queue (x Seq.:<| xs)) = Just (x, Queue xs)

appendQueue :: Queue a -> a -> Queue a
appendQueue (Queue xs) x = Queue $ xs Seq.:|> x

successors :: (Eq a) => a -> Queue a -> Maybe [a]
successors x (Queue xs) = go xs
  where
    go (y Seq.:<| ys)
      | x == y    = Just $ toList ys
      | otherwise = go ys
    go Seq.Empty  = Nothing

--------------------------------------------------
-- Branch worker
--------------------------------------------------

type WorkerM = StateT WorkerState IO

data WorkerState = WorkerState { _mergeQueue    :: Queue MergeRequestId
                               , _mergeRequests :: M.Map MergeRequestId MergeRequestState
                               , _failedMerges  :: [MergeRequestId]
                               , _branchHead    :: SHA
                               }

makeLenses ''MergeRequestState
makeLenses ''WorkerState

toBuildRef :: MergeRequestId -> Ref
toBuildRef (MergeRequestId n) = Ref $ "refs/heads/to-build/" <> T.pack (show n)

branchWorker :: Server -> ManagedBranch -> RpcChan BranchRequest -> IO ()
branchWorker server branch eventQueue = do
    let head = undefined -- TODO
    let s0 :: WorkerState
        s0 = WorkerState { _mergeQueue    = Queue Seq.Empty
                         , _mergeRequests = mempty
                         , _failedMerges  = []
                         , _branchHead    = head
                         }
    evalStateT go s0
  where
    go :: WorkerM ()
    go = forever $ do
        join $ uses mergeQueue $ mapM_ startPendingBuild
        _ <- runMaybeT mergeGoodRequests
        doEvent <- join $ uses id $ liftIO . atomically . getEvent
        doEvent

    startPendingBuild :: MergeRequestId -> WorkerM ()
    startPendingBuild reqId = do
        status <- use $ mr reqId . mergeStatus
        case status of
          PendingBuild -> do
              brHead <- use branchHead
              reqCommits <- use $ mr reqId . mergeOrigCommits
              commits <- liftIO $ withWorkingDir server $ \repo -> do
                  commits <- rebase repo reqCommits brHead -- TODO exception
                  let head' = headCommit commits
                  Git.push repo (Remote "origin") (CommitSha head') (toBuildRef reqId)
                  return commits

              branchHead .= headCommit commits
              builder <- liftIO $ async $ serverStartBuild server branch (headCommit commits)
              mr reqId . mergeStatus .= Building commits builder
          _ -> return ()

    mergeGoodRequests :: MaybeT WorkerM ()
    mergeGoodRequests = do
        queue <- lift $ use mergeQueue
        (reqId, rest) <- MaybeT $ pure $ popQueue queue
        Succeeded (CommitRange baseSha headSha) <- lift $ use $ mr reqId . mergeStatus
        let tryAgain :: SomeException -> MaybeT WorkerM ()
            tryAgain _ = do
                lift $ mr reqId . mergeStatus .= PendingBuild
                mzero
        handle tryAgain $
            liftIO $ updateRefs (serverRepo server)
                                [ UpdateRef (getManagedBranch branch) headSha (Just baseSha) ]
        lift $ mergeQueue .= rest
        mergeGoodRequests

    getEvent :: WorkerState -> STM (WorkerM ())
    getEvent s = getRequest <|> getBuildFinished
      where
        getRequest = matchRpc eventQueue $ \reply req -> do
            res <- handle (return . Left)
                  $ fmap Right $ handleBranchRequest req
            liftIO $ atomically $ reply res
        getBuildFinished = asum
            [ handleBuildFinished reqId rebasedCommits <$> waitSTM resultVar
            | reqId <- toList $ s ^. mergeQueue
            , let Just req = M.lookup reqId (_mergeRequests s)
            , Building rebasedCommits resultVar <- pure $ req ^. mergeStatus
            ]
      -- TODO: detect force push
      -- join $ uses mergeQueue $ mapM_ cancelBuild

    mr :: MergeRequestId -> Lens' WorkerState MergeRequestState
    mr i = mergeRequests . singular (ix i)

    -- Handling events
    handleBranchRequest :: BranchRequest a -> WorkerM a
    handleBranchRequest (NewMergeRequest {..}) = do
        reqId <- liftIO $ freshRequestId server
        mergeQueue %= flip appendQueue reqId
        mr reqId .= MergeRequestState { _mergeRequestId   = reqId
                                      , _mergeOrigCommits = newMergeReqCommits
                                      , _mergeStatus      = PendingBuild
                                      , _mergeBranch      = branch
                                      }
        return reqId
    handleBranchRequest (CancelMergeRequest {..}) = cancelBuild cancelMergeReqId
    handleBranchRequest (GetBranchStatus{}) =
        return undefined -- TODO

    handleBuildFinished :: MergeRequestId -> CommitRange -> BuildResult -> WorkerM ()
    handleBuildFinished reqId rebasedCommits (BuildFailed msg) = do
        req <- use $ mr reqId
        Just succs <- uses mergeQueue $ successors reqId
        mapM_ cancelBuild succs
        mr reqId . mergeStatus .= FailedToBuild rebasedCommits msg
        failedMerges %= (reqId :)
    handleBuildFinished reqId rebasedCommits BuildSucceeded = do
        mr reqId . mergeStatus .= Succeeded rebasedCommits

    -- Cancel any builds associated with a merge request and mark it as pending
    cancelBuild :: MergeRequestId -> WorkerM ()
    cancelBuild reqId = do
        req <- use $ mr reqId
        case req ^. mergeStatus of
          Building _ builder -> liftIO $ cancel builder
          _                  -> return ()
        mr reqId . mergeStatus .= PendingBuild

stateInvariant :: WorkerState -> Bool
stateInvariant state = True -- TODO
    -- branchHead == head of last MergeRequest
