{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- | Database manipulations.
module DB
  ( -- * Schema
    initializeDB
  , initializeRepoDB
  , GitLabMergeRequest(..)
  , insertGitLabMergeRequest
  , getGitLabMergeRequest
  , setLastCommentStatus
  ) where

import qualified Database.HDBC as HDBC
import Database.HDBC (withTransaction)
import qualified Database.HDBC.Sqlite3 as SQLite
import Data.FileEmbed (embedStringFile)
import Database.HDBC (SqlValue (..), fromSql, toSql)
import Database.YeshQL.HDBC (yesh1)
import Database.YeshQL.HDBC.SqlRow.TH (makeSqlRow)

import Autopush.MergeRequest
import Autopush.DB (withDB)
import Git (SHA, GitRepo (..))

initializeRepoDB :: GitRepo -> IO ()
initializeRepoDB g = initializeDB (gitRepoDir g)

-- | Set up an initial database schema.
initializeDB :: FilePath -> IO ()
initializeDB dirname = withDB dirname $ \conn ->
  withTransaction conn $ \conn ->
  HDBC.runRaw conn $(embedStringFile "schema.sql")

data GitLabMergeRequest = GitLabMergeRequest { glmrLastCommentStatus :: MergeRequestStatus
                                             , glmrOriginalHead      :: SHA
                                             }

makeSqlRow ''GitLabMergeRequest

[yesh1|
  -- name:insertGitLabMergeRequest :: ()
  -- :mrid   :: MergeRequestID
  -- :head   :: SHA
  -- :status :: MergeRequestStatus
  INSERT INTO gitlab_merge_requests (merge_request, original_head, last_comment_status)
  VALUES (:mrid, :head, :status)
|]

[yesh1|
  -- name:getGitLabMergeRequest :: GitLabMergeRequest
  -- :mrid :: MergeRequestID
  SELECT last_comment_status
  FROM gitlab_merge_requests
  WHERE merge_request = :mrid
|]

[yesh1|
  -- name:setLastCommentStatus :: rowcount Int
  -- :mrid :: MergeRequestID
  -- :status :: MergeRequestStatus
  UPDATE gitlab_merge_requests
  SET last_comment_status = :status
  WHERE merge_request = :mrid
|]

