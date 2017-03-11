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
module PushMerge
    ( startServer
    , Server
    , newMergeRequest
    , NewMergeRequestError(..)
    , isMergeBranch
      -- * Types
    , MergeRequestId
    , BranchNotManagedException(..)
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
import Data.Maybe
import Data.Semigroup
import Data.Foldable (toList, asum)
import System.Process
import System.Directory
import System.IO.Temp
import GHC.Generics

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Control.Lens

import RpcChannel
import PushMerge.Types
import Git
import Utils

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

    temp <- getTemporaryDirectory
    workingDirs <- replicateM 3 $ do
        dir <- createTempDirectory temp "repo"
        Git.clone serverRepo dir
    serverWorkingDirPool <- newTVarIO workingDirs
    return $ Server {..}

freshRequestId :: Server -> IO MergeRequestId
freshRequestId server = atomically $ do
    i <- readTVar $ serverNextRequestId server
    writeTVar (serverNextRequestId server) $ case i of MergeRequestId i -> MergeRequestId (i+1)
    return i

withWorkingDir :: Server -> (GitRepo -> IO a) -> IO a
withWorkingDir server action = do
    bracket acquire release action
  where
    pool = serverWorkingDirPool server
    acquire = atomically $ do
        xs <- readTVar pool
        case xs of
          [] -> retry
          x:xs' -> writeTVar pool xs' >> return x
    release dir = atomically $ modifyTVar pool (dir:)


data Server = Server { serverBranches       :: TVar (M.Map ManagedBranch (RpcChan BranchRequest))
                     , serverNextRequestId  :: TVar MergeRequestId
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
stateInvariant state =
    -- All requests in mergeQueue are in mergeRequests
    all (`M.member` view mergeRequests state) (state ^. mergeQueue)
    -- branchHead == head of last MergeRequest

toBuildRef :: MergeRequestId -> Ref
toBuildRef (MergeRequestId n) = Ref $ "refs/heads/to-build/" <> T.pack (show n)

branchWorker :: Server -> ManagedBranch -> RpcChan BranchRequest -> IO ()
branchWorker server branch eventQueue = do
    putStrLn $ "worker for "++show branch
    head <- resolveRef (serverRepo server) (getManagedBranch branch)
    putStrLn $ "HEAD is " ++ show head
    let s0 :: WorkerState
        s0 = WorkerState { _mergeQueue    = emptyQueue
                         , _mergeRequests = mempty
                         , _failedMerges  = []
                         , _branchHead    = head
                         }
    evalStateT go s0
  where
    go :: WorkerM ()
    go = forever $ do
        logMsg "worker go"
        join $ uses mergeQueue $ mapM_ startPendingBuild
        _ <- runMaybeT mergeGoodRequests
        logMsg "worker waiting"
        doEvent <- join $ uses id $ liftIO . atomically . getEvent
        logMsg "have event"
        doEvent

    startPendingBuild :: MergeRequestId -> WorkerM ()
    startPendingBuild reqId = do
        status <- use $ mr reqId . mergeStatus
        case status of
          PendingBuild -> do
              logMsg $ "starting build for "++show reqId
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
        lift $ logMsg $ "Trying to merge "++show reqId
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
        mergeRequests . at reqId ?=
            MergeRequestState { _mergeRequestId   = reqId
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

    logMsg :: String -> WorkerM ()
    logMsg = liftIO . putStrLn

--------------------------------------------------
-- Wrappers
--------------------------------------------------
data BranchNotManagedException = BranchNotManagedException
                               deriving (Exception, Show)

data NewMergeRequestError = CommitHasNoMergeBase
                          deriving (Show, Generic, Exception)
                          deriving anyclass (FromJSON, ToJSON)

newMergeRequest :: Server -> Ref -> CommitRange
                -> IO MergeRequestId
newMergeRequest server ref commits =
    branchRequest server ref
    $ NewMergeRequest { newMergeReqCommits = commits }

branchRequest :: Server -> Ref -> BranchRequest a -> IO a
branchRequest server ref req
  | Just managed <- isManagedBranch ref = do
        putStrLn "Request"
        r <- atomically $ do
            branches <- readTVar $ serverBranches server
            case M.lookup managed branches of
              Just chan -> return $ Right chan
              Nothing -> do chan <- newRpcChan
                            writeTVar (serverBranches server) $ M.insert managed chan branches
                            return $ Left chan
        case r of
          Right chan -> sendRpc chan req
          Left chan -> do
              putStrLn $ "Starting worker for "++show ref
              worker <- async $ branchWorker server managed chan
              link worker
              branchRequest server ref req
  | otherwise = throwM $ BranchNotManagedException
