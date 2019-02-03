{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Autopush.Actions
where

import Git
import Utils
import Autopush.DB
import Autopush.MergeRequest
import Autopush.MergeBranch
import Autopush.BuildDriver

import Control.Lens
import Control.Monad.Reader
import qualified Data.Text as Text
import Control.Concurrent.STM
import Database.HDBC.Sqlite3 as SQLite
import Control.Exception
import System.Directory
import System.FilePath

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
      , _buildDriver :: BuildDriver
      , _transactionDepth :: TVar Int
      }
makeLenses ''ActionContext

-- | The 'Action' monad.
-- Encapsulates leasing a working copy and running a database transaction.
type Action = ReaderT ActionContext IO

-- | Run an 'Action'.
runAction :: GitRepo -> TVar [GitRepo] -> BuildDriver -> Action a -> IO a
runAction srepo workingCopyPool bdriver action = do
  tdepthVar <- newTVarIO 0
  let context = ActionContext srepo workingCopyPool bdriver tdepthVar
  runReaderT action context

catchAction :: Exception e => Action a -> (e -> Action a) -> Action a
catchAction action handle = do
  context <- ask
  let actionIO = runReaderT action context
      handleIO e = runReaderT (handle e) context
  liftIO $ actionIO `catch` handleIO
  


mkWorkingCopies :: GitRepo -> FilePath -> Int -> IO (TVar [GitRepo])
mkWorkingCopies srepo dir num = do
  newTVarIO =<< (forM [1..num] $ \i -> do
    let wdir = dir </> "worker-" ++ show i ++ ".git"
    removePathForcibly wdir
    Git.clone srepo wdir)

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

-- | Transactionally run a database action. Supports a poor man's emulation of
-- nested transactions.
db :: (SQLite.Connection -> Action a) -> Action a
db action = do
  srepo <- view serverRepo
  context <- ask
  let actionIO = \conn -> runReaderT (action conn) context
  liftIO $ do
    withRepoDB srepo $ \conn -> do
      let acquire = atomically $ do
            t0 <- readTVar (context ^. transactionDepth)
            writeTVar (context ^. transactionDepth) (succ t0)
            return t0
          release _ = atomically $ do
            t1 <- readTVar (context ^. transactionDepth)
            let t0 = pred t1
            writeTVar (context ^. transactionDepth) t0
            return t0
      bracket
        acquire
        release
        $ \nestingDepth ->
            if nestingDepth == 0 then
              transactionally actionIO conn
            else
              actionIO conn

-- | Run a git command against the server repo.
withGitServer :: (GitRepo -> Action a) -> Action a
withGitServer action = view serverRepo >>= action

-- | Prepare a new merge request
prepareMR :: MergeRequestID -> Action ()
prepareMR mid = db $ \conn -> do
  m <- liftIO (getExistingMergeRequest mid conn)
  baseCommit <- withGitServer $ \repo -> liftIO $ do
    Git.updateRefs
      repo
      [ UpdateRef (toOrigRef (mrID m)) (mrOriginalHead m) Nothing ]
    Git.mergeBase
      repo
      (CommitRef . branchRef . upstreamBranch $ mrBranch m)
      (CommitSha $ mrOriginalHead m)
  liftIO $
    updateMergeRequest
      m { mrOriginalBase = Just baseCommit
        , mrCurrentBase = Just baseCommit
        }
      conn
  return ()

-- | Get the rebase target for a merge request. If the merge request has any
-- parents, use their current heads, otherwise, use the current branch head.
getRebaseTarget :: MergeRequest -> SQLite.Connection -> Action (SHA, Ref)
getRebaseTarget m conn = do
  -- If we have a parent MR, rebase onto that
  parentMB <- liftIO $ getMergeRequestParent m conn
  case parentMB of
    Just mb ->
      return (mrCurrentHead mb, toOrigRef (mrID mb))
    Nothing ->
      getGitHead m

-- | Gets the current head on the server repo (the one we want to merge into)
getGitHead :: MergeRequest -> Action (SHA, Ref)
getGitHead m = do
  let branch = upstreamBranch (mrBranch m)
  withGitServer $ \repo -> do
    sha <- liftIO $ Git.resolveRef repo $ Git.branchRef branch
    return (sha, branchRef branch)

-- | Schedule a merge request to be built
scheduleMR :: MergeRequestID -> Action ()
scheduleMR reqId = db $ \conn -> do
  m <- liftIO (getExistingMergeRequest reqId conn)
  when
    (mrRebased m /= NotRebased)
    (error "Merge request already rebased")
  when
    (mrMerged m == Merged)
    (error "Already merged")

  m <- liftIO (reparentMergeRequest m conn)

  (brHead, brRef) <- getRebaseTarget m conn

  origBase <- maybe (error "No merge base") pure $ mrOriginalBase m
  let origHead = mrOriginalHead m
      origCommits = CommitRange origBase origHead

  withWorkingCopy $ \repo -> liftIO $ do
    rebaseResult <- do
      -- Fetch and rebase branch
      Git.fetch repo originRemote [toOrigRef reqId]
      -- Git.fetch repo originRemote [brRef]

      -- Make sure we're in detached head state, otherwise git will try to
      -- rebase the current branch, which might be nonsensical
      Git.checkout repo True (CommitSha brHead)
      logMsg $ "Fetched"
      let handleRebaseFail e@GitException{} = do
              logMsg $ "Failed to rebase " ++ show reqId ++ ": " ++ show e
              Git.abortRebase repo
              return Nothing
      handle handleRebaseFail $ fmap Just $ Git.rebase repo origCommits brHead

    case rebaseResult of
      Just commits -> do
        let head' = headCommit commits
            base' = baseCommit commits
        logMsg $ "Rebased " ++ show reqId ++ ": " ++ show commits

        -- Push rebased commits
        Git.push repo originRemote (CommitSha head') (toBuildRef reqId)
        logMsg $ "Pushed rebase " ++ show reqId ++ ": " ++ show commits

        updateMergeRequest
          m { mrRebased = Rebased
            , mrCurrentHead = head'
            , mrCurrentBase = Just base'
            }
          conn

      Nothing ->
        updateMergeRequest m { mrRebased = RebaseFailed } conn

startBuild :: MergeRequestID -> Action ()
startBuild reqId = do
  let buildRef = toBuildRef reqId
  start <- view $ buildDriver . buildStart
  db $ \conn -> do
    m <- liftIO $ getExistingMergeRequest reqId conn
    buildID <- withGitServer $ \repo -> liftIO $ do
      start repo buildRef
    liftIO $
      updateMergeRequest
        m { mrBuildID = Just buildID
          , mrMergeRequestStatus = Running
          }
        conn
  return ()

requeue jobID reqID = db $ \conn -> liftIO $ do
  finishJob jobID conn
  pushJob reqID conn
  return ()

done jobID = db $ \conn -> liftIO $ do
  finishJob jobID conn

runNextJob :: WorkerID -> Action ()
runNextJob worker = do
  (db $ liftIO . popJob worker) >>= \case
    Nothing ->
      -- nothing to do
      return ()
    Just (jobID, m) -> do
      execJob worker jobID m

runAllJobs :: WorkerID -> Action ()
runAllJobs worker = do
  (db $ liftIO . popJob worker) >>= \case
    Nothing ->
      -- nothing to do
      return ()
    Just (jobID, m) -> do
      execJob worker jobID m
      runAllJobs worker

execJob :: WorkerID -> JobID -> MergeRequest -> Action ()
execJob worker jobID m = do
    let reqId = mrID m
    liftIO . logMsg $ "Starting job " ++ show jobID ++ ", MR: " ++ show reqId
    liftIO . logMsg $ show m
    case (mrOriginalBase m, mrRebased m, mrMergeRequestStatus m, mrBuildID m) of
      (Nothing, _, _, _) -> do
        -- fresh MR, need to prepare it
        prepareMR reqId
        requeue jobID reqId

      (_, NotRebased, _, _) -> do
        -- needs to be reparented and/or rebased
        scheduleMR reqId
        requeue jobID reqId

      (_, _, Runnable, _) -> do
        -- It's runnable, let's run it
        startBuild reqId
        requeue jobID reqId

      (_, _, MergeCancelled, _) -> do
        -- We were asked to cancel the merge, abort it.
        db $ handleFailedBuild m

      (_, _, Running, Just buildID) -> do
        -- Already running, check status
        liftIO $ logMsg $
          "Checking build status for " ++
          Text.unpack (unBuildID buildID)
        check <- view $ buildDriver . buildStatus
        newStatus <- liftIO (check buildID)
        liftIO $ logMsg $
          "New status: " ++ show newStatus
        case newStatus of
          BuildFailed msg -> do
            liftIO $ logMsg $
              "*** Build failure ***\n" ++
              "Build: " ++ Text.unpack (unBuildID buildID) ++ "\n" ++
              "MR: " ++ show reqId ++ "\n" ++
              msg
            db $ handleFailedBuild m
          BuildCancelled -> do
            liftIO $ logMsg $
              "*** Build cancelled ***\n" ++
              "Build: " ++ Text.unpack (unBuildID buildID) ++ "\n" ++
              "MR: " ++ show reqId
            db $ handleFailedBuild m
          BuildPassed -> do
            db $ handlePassedBuild m
            requeue jobID reqId
          _ ->
            requeue jobID reqId

      (_, _, Running, Nothing) -> do
        -- No idea what's going on here; log & skip
        liftIO $ logMsg $ "No build found for MR " ++ show reqId
        db $
          liftIO .
            updateMergeRequest m { mrMergeRequestStatus = Runnable }
        requeue jobID reqId

      (_, _, Passed, _) -> do
        liftIO $ logMsg $ "Build passed, trying to merge " ++ show reqId
        db $ checkMergeableBuild m

      _ ->
        db $ liftIO . finishJob jobID

checkMergeableBuild :: MergeRequest -> SQLite.Connection -> Action ()
checkMergeableBuild m conn = do
  let reqId = mrID m
  pMay <- liftIO $ getMergeRequestParent m conn
  case pMay of
    Nothing -> do
      liftIO $ logMsg $ "No parent, merging " ++ show reqId
      mergeGoodRequest m conn
    Just p -> do
      if mrMerged p == Merged then do
        liftIO $ logMsg $ "Parent merged, merging " ++ show reqId
        mergeGoodRequest m conn
      else do
        liftIO $ logMsg $ "Parent not merged, requeue " ++ show reqId
        void $ liftIO $ pushJob (mrID m) conn

handlePassedBuild :: MergeRequest -> SQLite.Connection -> Action ()
handlePassedBuild m conn = do
  liftIO $ updateMergeRequest
    m { mrMergeRequestStatus = Passed
      , mrBuildID = Nothing
      }
    conn

handleFailedBuild :: MergeRequest -> SQLite.Connection -> Action ()
handleFailedBuild m conn = do
  cancelBuild <- view $ buildDriver . buildCancel
  let go :: MergeRequest -> MergeRequestStatus -> Action ()
      go m newStatus = do
        liftIO $
          updateMergeRequest
            m { mrParent = Nothing
              , mrMergeRequestStatus = newStatus
              , mrBuildID = Nothing
              }
            conn
        -- remove all job queue entries
        liftIO $ cancelJobsFor (mrID m) conn
        -- recurse if children exist
        liftIO (getMergeRequestChild m conn) >>= \case
          Nothing -> return ()
          Just p -> do
            liftIO $ maybe (return ()) cancelBuild (mrBuildID p)
            go p FailedDeps
  go m FailedBuild

mergeGoodRequest :: MergeRequest -> SQLite.Connection -> Action ()
mergeGoodRequest m conn = do
  flip catchAction handle $ withGitServer $ \repo -> liftIO $ do
    updateRefs repo
      [ UpdateRef (branchRef . upstreamBranch $ mrBranch m) (mrCurrentHead m) (mrCurrentBase m)
      , DeleteRef (toBuildRef $ mrID m) Nothing
      , DeleteRef (toOrigRef $ mrID m) Nothing
      ]
    updateMergeRequest
      m { mrMerged = Merged }
      conn
  where
    handle :: SomeException -> Action ()
    handle e = liftIO $ do
      logMsg (show e)
      updateMergeRequest
        m { mrMerged = MergeFailed }
        conn
