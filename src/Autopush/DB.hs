{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database manipulations.
module Autopush.DB
( withDB
, initializeDB
, createMergeRequest
, updateMergeRequest
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
import Git ( SHA(..), Branch (..) )

-- | Transactionally run some database code against an autopush database in
-- the indicated directory.
withDB :: FilePath -> (SQLite.Connection -> IO a) -> IO a
withDB dirname action = do
  bracket
    (SQLite.connectSqlite3 $ dirname </> "autopush.sqlite3")
    HDBC.disconnect
    (flip HDBC.withTransaction action)

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
  -- :branch :: Branch
  -- :base :: SHA
  -- :head :: SHA
  INSERT INTO merge_requests
    ( branch, orig_base, orig_head, current_head, parent )
    VALUES
    ( :branch
    , :base
    , :head
    , :head
    , ( SELECT id FROM merge_requests
        WHERE merged = 0
        ORDER BY id DESC
        LIMIT 1
      )
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

-- | Get the newest active merge request. This should give the same result as
-- what 'createMergeRequest_' auto-assigns as the parent when inserting a new
-- request.
[yesh1|
  -- name:getNewestActiveMergeRequest :: MergeRequest
  SELECT id, parent, status, rebased, branch, orig_base, orig_head, current_head, merged
    FROM merge_requests
    WHERE merged = 0
    ORDER BY id DESC
    LIMIT 1
|]

-- | Create a new merge request for a given SHA.
createMergeRequest :: Branch -> SHA -> SHA -> SQLite.Connection -> IO (Maybe MergeRequest)
createMergeRequest branch head base conn = HDBC.withTransaction conn $ \conn -> do
  rowcount <- createMergeRequest_ branch head base conn
  when (rowcount /= 1) (error "Insert failed")
  insertID <- maybe (error "Failed to get inserted ID") pure =<< lastMergeRequestID_ conn
  getMergeRequest insertID conn

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
