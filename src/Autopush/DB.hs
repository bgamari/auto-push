{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database manipulations.
module Autopush.DB
( withDB
, withRepoDB
, initializeDB
, initializeRepoDB
, createMergeRequest
, updateMergeRequest
, reparentMergeRequest
, bailMergeRequest
, getMergeRequest
, getActionableMergeRequests
, getMergeRequestEffectiveStatus
)
where

import qualified Database.HDBC as HDBC
import qualified Database.HDBC.Sqlite3 as SQLite
import Data.FileEmbed (embedStringFile)
import Database.YeshQL.HDBC (yesh, yesh1)
import Control.Exception (bracket)
import Control.Monad (when)
import System.FilePath

import Autopush.MergeRequest
import Autopush.MergeBranch
import Git ( SHA(..), Branch (..), GitRepo (..))

withRepoDB :: GitRepo -> (SQLite.Connection -> IO a) -> IO a
withRepoDB g = withDB (gitRepoDir g)

-- | Transactionally run some database code against an autopush database in
-- the indicated directory.
withDB :: FilePath -> (SQLite.Connection -> IO a) -> IO a
withDB dirname action = do
  bracket
    (SQLite.connectSqlite3 $ dirname </> "autopush.sqlite3")
    HDBC.disconnect
    (flip HDBC.withTransaction action)

initializeRepoDB :: GitRepo -> IO ()
initializeRepoDB g = initializeDB (gitRepoDir g)

-- | Set up an initial database schema.
initializeDB :: FilePath -> IO ()
initializeDB dirname = withDB dirname $ \conn ->
  HDBC.runRaw conn $(embedStringFile "schema.sql")

-- | Fetch the last insert-ID (SQLite-ism)
[yesh1|
  -- name:lastMergeRequestID_ :: (MergeRequestID)
  SELECT last_insert_rowid() FROM merge_requests
|]

-- | Low-level create a 'MergeRequest'. Automatically assigns parent.
[yesh1|
  -- name:createMergeRequest_ :: rowcount Integer
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
  SELECT id, parent, status, rebased, branch, orig_base, orig_head, current_head, merged
    FROM merge_requests
    WHERE id = :mrID
|]

-- | Get a 'MergeRequest's parent.
[yesh1|
  -- name:getMergeRequestParent :: MergeRequest
  -- :m :: MergeRequest
  SELECT id, parent, status, rebased, branch, orig_base, orig_head, current_head, merged
    FROM merge_requests
    WHERE id = :m.mrParent
|]

-- | Get the newest active merge request. The resulting merge request is the
-- one that should become the dependency of the next merge request to be
-- scheduled for rebasing and building.
[yesh1|
  -- name:getNewestActiveMergeRequest :: MergeRequest
  SELECT id, parent, status, rebased, branch, orig_base, orig_head, current_head, merged
    FROM merge_requests
    WHERE merged = 0
      AND rebased > 0
    ORDER BY id DESC
    LIMIT 1
|]

-- | Low-level reparenting.
[yesh1|
  -- name:reparentMergeRequest_ :: rowcount Integer
  -- :m :: MergeRequest
  UPDATE merge_requests
  SET parent =
        (
          SELECT id
            FROM merge_requests p
              WHERE merged = 0
                AND rebased > 0
                AND branch = :m.mrBranch
                AND NOT EXISTS
                  (
                    SELECT 1
                      FROM merge_requests c
                      WHERE c.parent = p.id
                  )
              ORDER BY id DESC LIMIT 1
        )
    , merged = 0
  WHERE id = :m.mrID
  AND :m.mrParent IS NULL
  LIMIT 1
|]

-- | Reparenting. This will find the newest active merge request, and
-- make the given one depend on it.
reparentMergeRequest :: MergeRequest -> SQLite.Connection -> IO MergeRequest
reparentMergeRequest m conn = HDBC.withTransaction conn $ \conn -> do
  rowcount <- reparentMergeRequest_ m conn
  when (rowcount /= 1) (error "Failed to reparent merge request")
  getMergeRequest (mrID m) conn >>= maybe (error "Updated merge request does not exist") pure

-- | Create a new merge request for a given SHA.
createMergeRequest :: ManagedBranch -> SHA -> SQLite.Connection -> IO (Maybe MergeRequest)
createMergeRequest branch head conn = HDBC.withTransaction conn $ \conn -> do
  rowcount <- createMergeRequest_ branch head conn
  when (rowcount /= 1) (error "Insert failed")
  insertID <- maybe (error "Failed to get inserted ID") pure =<< lastMergeRequestID_ conn
  getMergeRequest insertID conn

-- | Reset a merge request to \"pristine\" state (as if it had just been
-- created).
[yesh1|
  -- name:bailMergeRequest :: rowcount Integer
  -- :m :: MergeRequest
  UPDATE merge_requests
  SET parent = NULL
    , merged = 0
  WHERE id = :m.mrID
  LIMIT 1
|]

[yesh1|
  -- name:updateMergeRequest_ :: rowcount Integer
  -- :m :: MergeRequest
  UPDATE merge_requests
    SET parent = :m.mrParent
      , status = :m.mrBuildStatus
      , rebased = :m.mrRebased
      , orig_base = :m.mrOriginalBase
      , orig_head = :m.mrOriginalHead
      , current_head = :m.mrCurrentHead
      , merged = :m.mrMerged
    WHERE id = :m.mrID
    LIMIT 1
|]

updateMergeRequest :: MergeRequest -> SQLite.Connection -> IO ()
updateMergeRequest m conn = HDBC.withTransaction conn $ \conn -> do
  rowcount <- updateMergeRequest_ m conn
  when (rowcount /= 1) (error "Update failed, merge request may not exist")
  return ()

[yesh1|
  -- name:getActionableMergeRequests :: [MergeRequest]
  SELECT id, parent, status, rebased, branch, orig_base, orig_head, current_head, merged
    FROM merge_requests
    WHERE merged = 0
|]

getMergeRequestEffectiveStatus :: MergeRequest -> SQLite.Connection -> IO BuildStatus
getMergeRequestEffectiveStatus m conn = HDBC.withTransaction conn $ \conn -> go m
  where
    go m
      | isFailedStatus (mrBuildStatus m) =
          return (mrBuildStatus m)
      | mrParent m == Nothing =
          return (mrBuildStatus m)
      | otherwise = do
          parentMay <- getMergeRequestParent m conn
          case parentMay of
            Nothing ->
              return FailedDeps
            Just parent -> do
              parentStatus <- go parent
              return $ applyParentStatus parentStatus (mrBuildStatus m)
