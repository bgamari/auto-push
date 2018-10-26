{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
module Autopush.Actions
where

import Git
import Utils
import Autopush.DB
import Autopush.MergeRequest
import Autopush.MergeBranch

import Control.Lens
import Control.Lens.TH
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Concurrent.STM
import Database.HDBC.Sqlite3 as SQLite
import Data.Maybe
import Control.Exception

-- | The 'origin' remote
originRemote :: Remote
originRemote = Remote "origin"

-- | The ref which points to the original commits of a merge request.
toOrigRef :: MergeRequestID -> Ref
toOrigRef n = Ref $ "refs/heads/auto-push/orig/" <> Text.pack (show n)

-- | The ref which points to the most recent rebased commits of a merge request.
toBuildRef :: MergeRequestID -> Ref
toBuildRef n = Ref $ "refs/heads/auto-push/to-build/" <> Text.pack (show n)

data ActionContext
  = ActionContext
      { _serverRepo :: GitRepo
      , _workingCopyPool :: TVar [GitRepo]
      }
makeLenses ''ActionContext

-- | The 'Action' monad.
-- Encapsulates leasing a working copy and running a database transaction.
type Action = ReaderT ActionContext IO

-- | Run an 'Action'.
runAction :: GitRepo -> TVar [GitRepo] -> Action a -> IO a
runAction srepo workingCopyPool action = do
  let context = ActionContext srepo workingCopyPool
  runReaderT action context

-- | Run an IO action against an exclusively-leased working copy from a pool.
withWorkingCopyIO :: TVar [GitRepo] -> (GitRepo -> IO a) -> IO a
withWorkingCopyIO pool =
  bracket acquire release
  where
    acquire = atomically $ do
      copies <- readTVar pool
      case copies of
        [] -> retry
        (x:xs) -> do
          writeTVar pool xs
          return x
    release x = atomically $ do
      xs <- readTVar pool
      writeTVar pool (x:xs)

-- | Run an Action against an exclusively-leased working copy.
withWorkingCopy :: (GitRepo -> Action a) -> Action a
withWorkingCopy action = do
  pool <- view workingCopyPool
  context <- ask
  let actionIO = \repo -> runReaderT (action repo) context
  liftIO $ withWorkingCopyIO pool actionIO


-- | Transactionally run a database action.
db :: (SQLite.Connection -> Action a) -> Action a
db action = do
  srepo <- view serverRepo
  context <- ask
  let actionIO = \conn -> runReaderT (action conn) context
  liftIO $ withRepoDB srepo actionIO

-- | Run a git command against the server repo.
withGitServer :: (GitRepo -> Action a) -> Action a
withGitServer action = view serverRepo >>= action

-- | Prepare a new merge request
prepareMR :: MergeRequestID -> Action ()
prepareMR mid = db $ \conn -> do
  m <- maybe (error "Merge request doesn't exist") pure
        =<< liftIO (getMergeRequest mid conn)
  baseCommit <- withGitServer $ \repo -> liftIO $ do
    Git.updateRefs
      repo
      [ UpdateRef (toOrigRef (mrID m)) (mrOriginalHead m) Nothing ]
    Git.mergeBase
      repo
      (CommitRef . branchRef . upstreamBranch $ mrBranch m)
      (CommitSha $ mrOriginalHead m)
  liftIO $ updateMergeRequest m { mrOriginalBase = Just baseCommit } conn
  return ()

-- | Get the rebase target for a merge request. If the merge request has any
-- parents, use their current heads, otherwise, use the current branch head.
getRebaseTarget :: MergeRequest -> SQLite.Connection -> Action SHA
getRebaseTarget m conn = do
  -- If we have a parent MR, rebase onto that
  parentMB <- case mrParent m of
    Just parentID -> do
      parentMay <- liftIO $ getMergeRequest parentID conn
      case parentMay of
        Just parent -> return . Just $ mrCurrentHead parent
        Nothing -> return Nothing
    Nothing -> return Nothing
  case parentMB of
    Just mb -> return mb
    Nothing -> getGitHead m

-- | Gets the current head on the server repo (the one we want to merge into)
getGitHead :: MergeRequest -> Action SHA
getGitHead m = do
  let branch = upstreamBranch (mrBranch m)
  withGitServer $ \repo ->
    liftIO $ Git.resolveRef repo $ Git.branchRef branch

-- | Schedule a merge request to be built
scheduleMR :: MergeRequestID -> Action ()
scheduleMR reqId = db $ \conn -> do
  m <- maybe (error "Merge request doesn't exist") pure
        =<< liftIO (getMergeRequest reqId conn)
  when
    (mrRebased m /= NotRebased)
    (error "Merge request already rebased")
  when
    (mrMerged m == Merged)
    (error "Already merged")

  liftIO (reparentMergeRequest m conn)

  brHead <- getRebaseTarget m conn

  origBase <- maybe (error "No merge base") pure $ mrOriginalBase m
  let origHead = mrOriginalHead m
      origCommits = CommitRange origBase origHead

  withWorkingCopy $ \repo -> liftIO $ do
    rebaseResult <- do
      -- Fetch and rebase branch
      Git.fetch repo originRemote [toOrigRef reqId]
      logMsg $ "Fetched"
      let handleRebaseFail e@GitException{} = do
              logMsg $ "Failed to rebase " ++ show reqId ++ ": " ++ show e
              Git.abortRebase repo
              return Nothing
      handle handleRebaseFail $ fmap Just $ Git.rebase repo origCommits brHead

    case rebaseResult of
      Just commits -> do
        let head' = headCommit commits
        logMsg $ "Rebased " ++ show reqId ++ ": " ++ show commits

        updateMergeRequest m { mrRebased = Rebased, mrCurrentHead = head' } conn

        -- Push rebased commits
        Git.push repo originRemote (CommitSha head') (toBuildRef reqId)
        logMsg $ "Pushed rebase " ++ show reqId ++ ": " ++ show commits

      Nothing ->
        updateMergeRequest m { mrRebased = RebaseFailed } conn

-- TODO:
-- - schedule a merge request:
--    - find parent
--    - assign parent
--    - rebase
--    - mark as rebased
--
-- - bail:
--    - cancel CI build, if any
--    - reset to pristine state
--
-- - start a build
--    - tell CI to build this
--    - mark as running
--
-- - check on an active MR:
--    - check parent
--        - if parent failed or bailed:
--            - bail
--    - if running:
--        - get build status from CI
--            - if failed:
--                - mark failed
--            - if passed:
--                - mark passed
--    - if passed:
--        - if parent passed:
--            - merge
