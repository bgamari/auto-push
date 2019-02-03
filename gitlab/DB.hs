{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Convertible (Convertible, safeConvert, convError)

import Autopush.MergeRequest
import Autopush.DB (withDB, assertOneRow)
import Git (SHA, GitRepo (..))
import qualified GitLab.Common as GL
import GitLab.Common (MergeRequestId)

initializeRepoDB :: GitRepo -> IO ()
initializeRepoDB g = initializeDB (gitRepoDir g)

-- | Set up an initial database schema.
initializeDB :: FilePath -> IO ()
initializeDB dirname = withDB dirname $ \conn ->
  withTransaction conn $ \conn ->
  HDBC.runRaw conn $(embedStringFile "schema.sql")

-- Orphans
instance Convertible GL.MergeRequestId SqlValue where
  safeConvert (GL.MergeRequestId n) = pure $ SqlInteger $ fromIntegral n
instance Convertible SqlValue GL.MergeRequestId where
  safeConvert = fmap GL.MergeRequestId . safeConvert

data GitLabMergeRequest = GitLabMergeRequest { glmrMergeRequest          :: MergeRequestID
                                             , glmrLastCommentStatus     :: MergeRequestStatus
                                             , glmrGitLabMergeRequestId  :: GL.MergeRequestId
                                             , glmrOriginalHeadSha       :: SHA
                                             }
                        deriving (Show)

makeSqlRow ''GitLabMergeRequest

insertGitLabMergeRequest :: MergeRequestID 
                         -> GL.MergeRequestId 
                         -> SHA 
                         -> MergeRequestStatus 
                         -> SQLite.Connection -> IO ()
insertGitLabMergeRequest mrid gl_mr_id head status conn =
  assertOneRow $ insertGitLabMergeRequest_ mrid gl_mr_id head status conn

[yesh1|
  -- name:insertGitLabMergeRequest_ :: rowcount Int
  -- :mrid      :: MergeRequestID
  -- :gl_mr_id  :: MergeRequestId
  -- :head      :: SHA
  -- :status    :: MergeRequestStatus
  INSERT INTO gitlab_merge_requests (merge_request, last_comment_status, gitlab_merge_request_id, original_head_sha)
  VALUES (:mrid, :status, :gl_mr_id, :head)
|]

[yesh1|
  -- name:getGitLabMergeRequest :: GitLabMergeRequest
  -- :mrid :: MergeRequestId
  SELECT merge_request, last_comment_status, gitlab_merge_request_id, original_head_sha
  FROM gitlab_merge_requests
  WHERE gitlab_merge_request_id = :mrid
|]

setLastCommentStatus :: MergeRequestID -> MergeRequestStatus -> SQLite.Connection -> IO ()
setLastCommentStatus mrid status conn = 
  assertOneRow $ setLastCommentStatus_ mrid status conn

[yesh1|
  -- name:setLastCommentStatus_ :: rowcount Int
  -- :mrid :: MergeRequestID
  -- :status :: MergeRequestStatus
  UPDATE gitlab_merge_requests
  SET last_comment_status = :status
  WHERE merge_request = :mrid
|]

