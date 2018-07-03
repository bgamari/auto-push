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
--       ╰── D ── E   ⇐ feature-2
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
module PushMerge
    ( startServer
    , Server

      -- * Configuration
    , ServerConfig(..)
    , defaultIsMergeBranch

      -- * Defining builders
    , BuildAction
    , BuildResult(..)

      -- * Types
    , ManagedBranch
    , MergeRequestId

      -- * Requests
    , BranchNotManagedException(..)
    , newMergeRequest
    , NewMergeRequestError(..)
    , cancelMergeRequest
    , getBranchStatus
    , BranchStatus(..)
    , listBranches
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Trans.State
import Control.Monad.Catch
import Data.List (nub)
import Data.Semigroup
import Data.Foldable (toList, asum)
import System.Directory
import System.IO.Temp
import GHC.Generics
import Prelude hiding (head)

import qualified Data.Text as T
import qualified Data.Map as M

import Data.Aeson (FromJSON, ToJSON)
import Control.Lens

import RpcChannel
import PushMerge.Types
import Git
import Utils

originRemote :: Remote
originRemote = Remote "origin"

data ServerConfig = ServerConfig { repo :: GitRepo
                                 , builder :: BuildAction
                                 , isMergeBranch :: Branch -> Maybe ManagedBranch
                                 }

defaultIsMergeBranch :: Branch -> Maybe ManagedBranch
defaultIsMergeBranch (Branch ref) =
    ManagedBranch . Branch <$> T.stripPrefix "merge/" ref

startServer :: ServerConfig -> IO Server
startServer config = do
    serverBranches <- newTVarIO mempty

    -- Create some working directories for our pool
    temp <- getTemporaryDirectory
    workingDirs <- replicateM 3 $ do
        dir <- createTempDirectory temp "repo"
        Git.clone (repo config) dir
    serverWorkingDirPool <- newTVarIO workingDirs

    return $ Server { serverStartBuild = const $ builder config
                    , serverRepo = repo config
                    , serverIsMergeBranch = isMergeBranch config
                    , ..
                    }

withWorkingDir :: (MonadIO m, MonadMask m) => Server -> (GitRepo -> m a) -> m a
withWorkingDir server action = do
    bracket acquire release $ \repo -> do
        liftIO $ Git.remoteUpdate repo originRemote
        action repo
  where
    pool = serverWorkingDirPool server
    acquire = liftIO $ atomically $ do
        xs <- readTVar pool
        case xs of
          [] -> retry
          x:xs' -> writeTVar pool xs' >> return x
    release dir = liftIO $ atomically $ modifyTVar pool (dir:)


data Server = Server { serverBranches       :: TVar (M.Map ManagedBranch (RpcChan BranchRequest, TVar MergeRequestId))
                     , serverIsMergeBranch  :: Branch -> Maybe ManagedBranch
                     , serverStartBuild     :: ManagedBranch -> BuildAction
                     , serverRepo           :: GitRepo
                     , serverWorkingDirPool :: TVar [GitRepo]
                     }

--------------------------------------------------
-- Branch worker
--------------------------------------------------

type WorkerM = StateT WorkerState IO

data WorkerState = WorkerState { _mergeQueue    :: Queue MergeRequestId
                               , _mergeRequests :: M.Map MergeRequestId MergeRequestState
                               , _failedMerges  :: [MergeRequestId]
                               , _branchHead    :: SHA
                               }

makeLenses ''WorkerState

stateInvariant :: WorkerState -> Bool
stateInvariant s =
    -- All requests in mergeQueue are in mergeRequests
    all (`M.member` view mergeRequests s) (s ^. mergeQueue)
    -- TODO: branchHead == head of last MergeRequest
    -- Each request only occurs in queue once
    && let queue = toList $ s ^. mergeQueue
       in queue == nub queue


-- | The ref which points to the original commits of a merge request.
toOrigRef :: MergeRequestId -> Ref
toOrigRef (MergeRequestId n) = Ref $ "refs/heads/auto-push/orig/" <> T.pack (show n)

-- | The ref which points to the most recent rebased commits of a merge request.
toBuildRef :: MergeRequestId -> Ref
toBuildRef (MergeRequestId n) = Ref $ "refs/heads/auto-push/to-build/" <> T.pack (show n)

branchWorker :: Server -> ManagedBranch -> RpcChan BranchRequest -> IO ()
branchWorker server branch eventQueue = do
    putStrLn $ "worker for "++show branch
    head <- resolveRef (serverRepo server) (branchRef $ upstreamBranch branch)
    putStrLn $ "HEAD is " ++ show head
    let s0 :: WorkerState
        s0 = WorkerState { _mergeQueue    = emptyQueue
                         , _mergeRequests = mempty
                         , _failedMerges  = []
                         , _branchHead    = head
                         }
    evalStateT (forever go) s0
  where
    onException exc = logMsg $ "Exception: "++show exc

    go :: WorkerM ()
    go = handleAll onException $ do
        join $ uses mergeQueue $ \q -> logMsg $ "Merge queue: "++show (toList q)
        q <- use mergeQueue
        logMsg $ "Merge queue: "++show (toList q)
        _ <- join $ uses mergeQueue $ runMaybeT . mapM_ startPendingBuild
        mergeGoodRequests
        logMsg "worker waiting"
        doEvent <- join $ uses id $ liftIO . atomically . getEvent
        logMsg "have event"
        doEvent

    startPendingBuild :: MergeRequestId -> MaybeT WorkerM ()
    startPendingBuild reqId = do
        status <- lift $ use $ mrStatus reqId
        case status of
          PendingBuild -> do
              lift $ logMsg $ "starting build for "++show reqId
              brHead <- lift $ use branchHead
              reqCommits <- lift $ use $ mr reqId . mergeReqOrigCommits
              commits <- MaybeT $ withWorkingDir server $ \repo -> runMaybeT $ do
                  -- Fetch and rebase branch
                  liftIO $ Git.fetch repo originRemote [toOrigRef reqId]
                  lift $ logMsg $ "Fetched"

                  -- Figure out base commit
                  baseCommit <- liftIO $ mergeBase repo (CommitSha $ baseCommit reqCommits) (CommitSha brHead)
                  let rebaseRange = CommitRange baseCommit (headCommit reqCommits)
                  lift $ logMsg $ "Rebasing commits "++show rebaseRange

                  -- Rebase
                  let handleRebaseFail e@GitException{} = do
                          logMsg $ "Failed to rebase "++show reqId++": "++show e
                          liftIO $ Git.abortRebase repo
                          mrStatus reqId .= FailedToRebase brHead
                          return Nothing
                  commits <- MaybeT $ handle handleRebaseFail
                             $ fmap Just $ liftIO $ Git.rebase repo rebaseRange brHead
                  let head' = headCommit commits
                  lift $ logMsg $ "Rebased "++show reqId++": "++show commits

                  -- Push rebased commits
                  lift $ logMsg $ show (head', toBuildRef reqId)
                  lift $ logMsg "hello"
                  liftIO $ Git.push repo originRemote (CommitSha head') (toBuildRef reqId)
                  lift $ logMsg $ "Pushed rebase "++show reqId++": "++show commits
                  return commits

              lift $ branchHead .= headCommit commits
              let handleBuildException exc = do
                      Utils.logMsg $ show reqId++" failed with exception: "++show exc
                      pure $ BuildFailed $ show exc
              builder <- liftIO $ async $ handleAll handleBuildException
                         $ serverStartBuild server branch (headCommit commits)
              lift $ mrStatus reqId .= Building commits builder
              lift $ logMsg $ show reqId++" is now building"
          _ -> return ()

    mergeGoodRequests :: WorkerM ()
    mergeGoodRequests = void $ runMaybeT $ do
        queue <- lift $ use mergeQueue
        (reqId, rest) <- MaybeT $ pure $ popQueue queue
        status <- lift $ use $ mrStatus reqId
        case status of
          Succeeded (CommitRange baseSha headSha) -> do
              lift $ logMsg $ "Trying to merge "++show reqId
              let tryAgain :: SomeException -> MaybeT WorkerM ()
                  tryAgain exc = do
                      lift $ logMsg $ "Failed to merge "++show reqId++": "++show exc
                      lift $ mrStatus reqId .= PendingBuild
                      mzero
              handle tryAgain $
                  liftIO $ updateRefs (serverRepo server)
                                      [ UpdateRef (branchRef $ upstreamBranch branch) headSha (Just baseSha)
                                      , DeleteRef (toBuildRef reqId) Nothing
                                      , DeleteRef (toOrigRef reqId) Nothing
                                      ]
              lift $ mergeQueue .= rest
              lift $ logMsg $ "Successfully merged "++show reqId
              lift $ mrStatus reqId .= Merged headSha
              lift $ mergeGoodRequests

          FailedToBuild _ err -> lift $ do
              logMsg $ "Giving up on "++show reqId++" due to build failure: "++err
              mergeQueue .= rest
              mapM_ cancelBuild rest
              go

          FailedToRebase _ -> lift $ do
              logMsg $ "Giving up on "++show reqId++" due to rebase failure"
              mergeQueue .= rest
              mapM_ cancelBuild rest
              go

          _ -> return ()

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
            , Building rebasedCommits resultVar <- pure $ req ^. mergeReqStatus
            ]
      -- TODO: detect force push
      -- join $ uses mergeQueue $ mapM_ cancelBuild

    mr :: MergeRequestId -> Lens' WorkerState MergeRequestState
    mr i = mergeRequests . singular (ix i)

    mrStatus :: MergeRequestId -> Lens' WorkerState (RequestStatus (Async BuildResult))
    mrStatus i = mr i . mergeReqStatus

    -- Handling events
    handleBranchRequest :: BranchRequest a -> WorkerM a
    handleBranchRequest (NewMergeRequest {..}) = do
        let reqId = newMergeReqId
        baseCommit <- liftIO $ Git.mergeBase (serverRepo server)
                                             (CommitRef $ branchRef $ upstreamBranch branch)
                                             (CommitSha newMergeReqHead)
        liftIO $ Git.updateRefs (serverRepo server)
                                [ UpdateRef (toOrigRef reqId) newMergeReqHead Nothing ]
        mergeQueue %= flip appendQueue reqId
        mergeRequests . at reqId ?=
            MergeRequestState { _mergeReqId          = reqId
                              , _mergeReqOrigCommits = CommitRange baseCommit newMergeReqHead
                              , _mergeReqStatus      = PendingBuild
                              , _mergeReqBranch      = branch
                              }
    handleBranchRequest (CancelMergeRequest {..}) = cancelBuild cancelMergeReqId
    handleBranchRequest (GetBranchStatus{}) =
        BranchStatus
            <$> use branchHead
            <*> uses mergeRequests (M.toList . fmap (views mergeReqStatus $ fmap $ const ()))

    handleBuildFinished :: MergeRequestId -> CommitRange -> BuildResult -> WorkerM ()
    handleBuildFinished reqId rebasedCommits (BuildFailed msg) = do
        logMsg $ "Merge request "++show reqId++" failed to build"
        invalidateSuccessorBuilds reqId
        mrStatus reqId .= FailedToBuild rebasedCommits msg
    handleBuildFinished reqId rebasedCommits BuildSucceeded = do
        logMsg $ "Merge request "++show reqId++" succeessfully built; ready for merge"
        mrStatus reqId .= Succeeded rebasedCommits

    invalidateSuccessorBuilds :: MergeRequestId -> WorkerM ()
    invalidateSuccessorBuilds reqId = do
        Just succs <- uses mergeQueue $ successors reqId
        mapM_ cancelBuild succs

    -- Cancel any builds associated with a merge request and mark it as pending
    cancelBuild :: MergeRequestId -> WorkerM ()
    cancelBuild reqId = do
        req <- use $ mr reqId
        case req ^. mergeReqStatus of
          Building _ builder -> do liftIO $ cancel builder
          _                  -> return ()
        mrStatus reqId .= PendingBuild

    logMsg :: String -> WorkerM ()
    logMsg = liftIO . Utils.logMsg . ("Worker: "++)

--------------------------------------------------
-- Wrappers
--------------------------------------------------
data BranchNotManagedException = BranchNotManagedException
                               deriving (Exception, Show)

branchRequest :: Server -> Branch
              -> BranchRequest a -> IO a
branchRequest server branch req = do
    (chan, _) <- getBranchChannel server branch
    putStrLn $ "Request " ++ show req
    sendRpc chan req

freshRequestId :: TVar MergeRequestId -> STM MergeRequestId
freshRequestId nextRequestId = do
    MergeRequestId n <- readTVar nextRequestId
    writeTVar nextRequestId (MergeRequestId $ succ n)
    return $ MergeRequestId n

getBranchChannel :: Server -> Branch
                 -> IO (RpcChan BranchRequest, TVar MergeRequestId)
getBranchChannel server branch
  | Just managed <- serverIsMergeBranch server branch = do
        (chan, nextRequestId, startIt) <-
            atomically $ getOrStartBranchWorker managed
        startIt
        return (chan, nextRequestId)
  | otherwise = throwM BranchNotManagedException
  where
    getOrStartBranchWorker
        :: ManagedBranch
        -> STM (RpcChan BranchRequest, TVar MergeRequestId, IO ())
    getOrStartBranchWorker managed = do
        branches <- readTVar $ serverBranches server
        case M.lookup managed branches of
          Just (chan, nextRequestId) -> do
              return (chan, nextRequestId, return ())
          Nothing -> do
              chan <- newRpcChan
              nextRequestId <- newTVar $ MergeRequestId 0
              writeTVar (serverBranches server)
                  $ M.insert managed (chan, nextRequestId) branches
              let startIt = do
                      putStrLn $ "Starting worker for "++show branch
                      worker <- async $ branchWorker server managed chan
                      link worker
              return (chan, nextRequestId, startIt)

data NewMergeRequestError = CommitHasNoMergeBase
                          deriving (Show, Generic, Exception)
                          deriving anyclass (FromJSON, ToJSON)

newMergeRequest :: Server -> Branch -> SHA
                -> IO MergeRequestId
newMergeRequest server branch headCommit = do
    (chan, nextRequestId) <- getBranchChannel server branch
    reqId <- atomically $ freshRequestId nextRequestId
    void $ async $ sendRpc chan
        $ NewMergeRequest { newMergeReqHead = headCommit
                          , newMergeReqId = reqId
                          }
    return reqId

cancelMergeRequest :: Server -> Branch -> MergeRequestId -> IO ()
cancelMergeRequest server branch reqId = do
    branchRequest server branch $ CancelMergeRequest reqId

getBranchStatus :: Server -> Branch -> IO BranchStatus
getBranchStatus server branch =
    branchRequest server branch $ GetBranchStatus

listBranches :: Server -> IO [ManagedBranch]
listBranches server =
    atomically $ fmap M.keys $ readTVar (serverBranches server)
