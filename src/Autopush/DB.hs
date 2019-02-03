{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- | Database manipulations.
module Autopush.DB
( 
-- * Connection and Transaction Management
  withDB
, withRepoDB
, withTransaction
, transactionally

-- * Schema
, initializeDB
, initializeRepoDB

-- * Merge Requests
, listMergeRequests
, createMergeRequest
, updateMergeRequest
, reparentMergeRequest
, bailMergeRequest
, cancelMergeRequest
, getMergeRequest
, getExistingMergeRequest
, getMergeRequestParent
, getMergeRequestChild
, getMergeRequestByHead
, getActionableMergeRequests
, getMergeRequestEffectiveStatus
, getNewestActiveMergeRequest
, resetBuildInfo

-- * Jobs
, pushJob
, popJob
, abandonJob
, finishJob
, listJobs
, cancelJobsFor


-- * Miscellaneous utilities
, assertOneRow
)
where

import qualified Database.HDBC as HDBC
import Database.HDBC (withTransaction)
import qualified Database.HDBC.Sqlite3 as SQLite
import Data.FileEmbed (embedStringFile)
import Database.YeshQL.HDBC (yesh1)
import Control.Exception (bracket, Exception, throw)
import Control.Monad (when)
import System.FilePath
import Control.Monad (void)

import Autopush.MergeRequest
import Autopush.MergeBranch
import Git ( SHA(..), GitRepo (..))

data RowNotFoundException = RowNotFoundException
  deriving (Show)

instance Exception RowNotFoundException where

data TooManyRowsException = TooManyRowsException
  deriving (Show)

instance Exception TooManyRowsException where

assertRow :: IO (Maybe a) -> IO a
assertRow = (maybe (throw RowNotFoundException) return =<<)

assertOneRow :: IO Int -> IO ()
assertOneRow action = do
  action >>= \case
    1 -> return ()
    0 -> throw RowNotFoundException
    _ -> throw TooManyRowsException

transactionally :: (SQLite.Connection -> IO a) -> SQLite.Connection -> IO a
transactionally = flip withTransaction

withRepoDB :: GitRepo -> (SQLite.Connection -> IO a) -> IO a
withRepoDB g = withDB (gitRepoDir g)

-- | Run some database code against an autopush database in
-- the indicated directory.
withDB :: FilePath -> (SQLite.Connection -> IO a) -> IO a
withDB dirname action = do
  bracket
    (SQLite.connectSqlite3 $ dirname </> "autopush.sqlite3")
    HDBC.disconnect
    action

initializeRepoDB :: GitRepo -> IO ()
initializeRepoDB g = initializeDB (gitRepoDir g)

-- | Set up an initial database schema.
initializeDB :: FilePath -> IO ()
initializeDB dirname = withDB dirname $ \conn ->
  withTransaction conn $ \conn ->
  HDBC.runRaw conn $(embedStringFile "schema.sql")


-- | Fetch the last insert-ID (SQLite-ism)
[yesh1|
  -- name:lastMergeRequestID_ :: (MergeRequestID)
  SELECT last_insert_rowid() FROM merge_requests
|]

-- | Low-level create a 'MergeRequest'. Automatically assigns parent.
[yesh1|
  -- name:createMergeRequest_ :: rowcount Int
  -- :branch :: ManagedBranch
  -- :head :: SHA
  INSERT INTO merge_requests
    ( branch, orig_head, current_head )
    VALUES
    ( :branch
    , :head
    , :head
    )
|]

-- | Get a 'MergeRequest' by ID.
[yesh1|
  -- name:getMergeRequest :: MergeRequest
  -- :mrID :: MergeRequestID
  SELECT id
       , parent
       , status
       , rebased
       , branch
       , orig_base
       , orig_head
       , current_base
       , current_head
       , merged
       , build_id
    FROM merge_requests
    WHERE id = :mrID
|]

-- | List merge request IDs
[yesh1|
  -- name:listMergeRequests :: [(MergeRequestID)]
  SELECT id
    FROM merge_requests
    ORDER BY id
|]

-- | Get a 'MergeRequest' by head commit ID
[yesh1|
  -- name:getMergeRequestByHead :: MergeRequest
  -- :head :: SHA
  SELECT id
       , parent
       , status
       , rebased
       , branch
       , orig_base
       , orig_head
       , current_base
       , current_head
       , merged
       , build_id
    FROM merge_requests
    WHERE orig_head = :head
|]

getExistingMergeRequest :: MergeRequestID -> SQLite.Connection -> IO MergeRequest
getExistingMergeRequest mid conn =
  assertRow $ getMergeRequest mid conn

-- | Get a 'MergeRequest's parent.
[yesh1|
  -- name:getMergeRequestParent :: MergeRequest
  -- :m :: MergeRequest
  SELECT id
       , parent
       , status
       , rebased
       , branch
       , orig_base
       , orig_head
       , current_base
       , current_head
       , merged
       , build_id
    FROM merge_requests
    WHERE id = :m.mrParent
|]

-- | Get a 'MergeRequest's child.
[yesh1|
  -- name:getMergeRequestChild :: MergeRequest
  -- :m :: MergeRequest
  SELECT id
       , parent
       , status
       , rebased
       , branch
       , orig_base
       , orig_head
       , current_base
       , current_head
       , merged
       , build_id
    FROM merge_requests
    WHERE parent = :m.mrID
|]

-- | Get the newest active merge request. The resulting merge request is the
-- one that should become the dependency of the next merge request to be
-- scheduled for rebasing and building.
[yesh1|
  -- name:getNewestActiveMergeRequest :: MergeRequest
  -- :branch :: ManagedBranch
  SELECT id
       , parent
       , status
       , rebased
       , branch
       , orig_base
       , orig_head
       , current_base
       , current_head
       , merged
       , build_id
    FROM merge_requests
    WHERE merged = 0
      AND rebased > 0
      AND branch = :branch
    ORDER BY id DESC
    LIMIT 1
|]

-- | Reparenting. This will find the newest active merge request, and
-- make the given one depend on it.
reparentMergeRequest :: MergeRequest -> SQLite.Connection -> IO MergeRequest
reparentMergeRequest m conn = do
  parent <- getNewestActiveMergeRequest (mrBranch m) conn
  let parentID = fmap mrID parent
  updateMergeRequest m { mrParent = parentID } conn
  getExistingMergeRequest (mrID m) conn

-- | Reset a merge request to \"pristine\" state (as if it had just been
-- created).
[yesh1|
  -- name:bailMergeRequest :: rowcount Int
  -- :m :: MergeRequest
  UPDATE merge_requests
  SET parent = NULL
    , merged = 0
    , status = 'Runnable'
  WHERE id = :m.mrID
|]

-- | Cancel a merge request.
[yesh1|
  -- name:cancelMergeRequest :: rowcount Int
  -- :m :: MergeRequest
  UPDATE merge_requests
  SET merged = 0
    , status = 'MergeCancelled'
  WHERE id = :m.mrID
|]

-- | Create a new merge request for a given SHA.
createMergeRequest :: ManagedBranch -> SHA -> SQLite.Connection -> IO MergeRequest
createMergeRequest branch head conn = do
  getMergeRequestByHead head conn >>= \case
    Just m -> do
      bailMergeRequest m conn
      return m
    Nothing -> do
      assertOneRow $ createMergeRequest_ branch head conn
      insertID <- assertRow $ lastMergeRequestID_ conn
      assertRow $ getMergeRequest insertID conn

[yesh1|
  -- name:updateMergeRequest_ :: rowcount Int
  -- :m :: MergeRequest
  UPDATE merge_requests
    SET parent = :m.mrParent
      , status = :m.mrMergeRequestStatus
      , rebased = :m.mrRebased
      , orig_base = :m.mrOriginalBase
      , orig_head = :m.mrOriginalHead
      , current_base = :m.mrCurrentBase
      , current_head = :m.mrCurrentHead
      , merged = :m.mrMerged
      , build_id = :m.mrBuildID
    WHERE id = :m.mrID
|]

updateMergeRequest :: MergeRequest -> SQLite.Connection -> IO ()
updateMergeRequest m conn = do
  rowcount <- updateMergeRequest_ m conn
  when (rowcount /= 1) (error "Update failed, merge request may not exist")
  return ()

[yesh1|
  -- name:getActionableMergeRequests :: [MergeRequest]
  SELECT id
       , parent
       , status
       , rebased
       , branch
       , orig_base
       , orig_head
       , current_base
       , current_head
       , merged
       , build_id
    FROM merge_requests
    WHERE merged = 0
|]

getMergeRequestEffectiveStatus :: MergeRequest -> SQLite.Connection -> IO MergeRequestStatus
getMergeRequestEffectiveStatus m conn = go m
  where
    go m
      | isFailedStatus (mrMergeRequestStatus m) =
          return (mrMergeRequestStatus m)
      | mrParent m == Nothing =
          return (mrMergeRequestStatus m)
      | otherwise = do
          parentMay <- getMergeRequestParent m conn
          case parentMay of
            Nothing ->
              return FailedDeps
            Just parent -> do
              parentStatus <- go parent
              return $ applyParentStatus parentStatus (mrMergeRequestStatus m)

-- | Fetch the last insert-ID (SQLite-ism)
[yesh1|
  -- name:lastJobID_ :: (JobID)
  SELECT last_insert_rowid() FROM jobs
|]

-- | Gets the next job from the job queue
[yesh1|
  -- name:getJob_ :: (JobID)
  SELECT id
  FROM jobs
  WHERE worker IS NULL
  ORDER BY id
  LIMIT 1
|]

[yesh1|
  -- name:getJobMR_ :: MergeRequest
  -- :jobID :: JobID
  SELECT m.id
       , m.parent
       , m.status
       , m.rebased
       , m.branch
       , m.orig_base
       , m.orig_head
       , m.current_base
       , m.current_head
       , m.merged
       , m.build_id
    FROM merge_requests m
    INNER JOIN jobs j ON j.merge_request = m.id
    WHERE j.id = :jobID
|]

[yesh1|
  -- name:claimJob_ :: rowcount Int
  -- :jobID :: JobID
  -- :worker :: WorkerID
  UPDATE jobs
    SET worker = :worker
    WHERE id = :jobID
|]

[yesh1|
  -- name:finishJob_ :: rowcount Int
  -- :jobID :: JobID
  DELETE FROM jobs
    WHERE id = :jobID
|]

[yesh1|
  -- name:cancelJobsFor :: rowcount Int
  -- :mrID :: MergeRequestID
  DELETE FROM jobs
    WHERE merge_request = :mrID
|]

[yesh1|
  -- name:dropJob_ :: rowcount Int
  -- :jobID :: JobID
  UPDATE jobs
    SET worker = NULL
    WHERE id = :jobID
|]

[yesh1|
  -- name:insertJob_ :: rowcount Int
  -- :mrID :: MergeRequestID
  INSERT INTO jobs
    (merge_request)
    VALUES
    (:mrID)
|]

type MaybeWorkerID = Maybe WorkerID

[yesh1|
  -- name:listJobs :: [(JobID, MergeRequestID, MaybeWorkerID)]
  SELECT id, merge_request, worker
  FROM jobs
  ORDER BY id
|]

[yesh1|
  -- name:resetBuildInfo :: rowcount Int
  UPDATE merge_requests
  SET build_id = NULL
|]

pushJob :: MergeRequestID -> SQLite.Connection -> IO JobID
pushJob mrID conn = do
  assertOneRow $ insertJob_ mrID conn
  assertRow $ lastJobID_ conn

popJob :: WorkerID -> SQLite.Connection -> IO (Maybe (JobID, MergeRequest))
popJob worker conn = do
  getJob_ conn >>= \case
    Nothing ->
      return Nothing
    Just jobID -> do
      assertOneRow $ claimJob_ jobID worker conn
      m <- assertRow $ getJobMR_ jobID conn
      return $ Just (jobID, m)

abandonJob :: JobID -> SQLite.Connection -> IO ()
abandonJob jobID conn = do
  assertOneRow $ dropJob_ jobID conn

finishJob :: JobID -> SQLite.Connection -> IO ()
finishJob jobID conn = do
  void $ finishJob_ jobID conn
