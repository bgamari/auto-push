{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines the data model for merge requests.
module Autopush.MergeRequest
where

import Git (SHA(..), Branch (..))
import Data.Text (Text)
import Data.Convertible (Convertible, safeConvert, convError)
import Text.Read (readMaybe)
import Database.HDBC (SqlValue (..), fromSql, toSql)
import Database.YeshQL.HDBC.SqlRow.TH (makeSqlRow)
import Database.YeshQL.HDBC.SqlRow.Class (SqlRow)
import Data.ByteString.UTF8 as UTF8

type MergeRequestID = Integer

-- | Represent a merge request and its current processing status.
data MergeRequest
  = MergeRequest
      { mrID :: MergeRequestID
      , mrParent :: Maybe MergeRequestID
        -- ^ Next up in the dependency chain.
      , mrBuildStatus :: BuildStatus
        -- ^ Own build status. Note that parent build status is implicit.
      , mrRebased :: RebaseStatus
        -- ^ What this MR is currently rebased on.
      , mrBranch :: Branch
        -- ^ Branch name
      , mrOriginalBase :: SHA
        -- ^ SHA of the original branch merge base
      , mrOriginalHead :: SHA
        -- ^ SHA of the original branch head
      , mrCurrentHead :: SHA
        -- ^ SHA of the current (rebased) branch head
      , mrMerged :: MergeStatus
        -- ^ Whether this MR has been merged.
      }
      deriving (Show, Eq)

-- | What a MR is rebased onto.
data RebaseStatus
  = NoBase -- ^ not rebased yet
  | RebasedDeps -- ^ rebased optimistically, onto a dependency
  | RebasedMaster -- ^ rebased onto master
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Convertible RebaseStatus SqlValue where
  safeConvert NoBase = pure $ SqlString "none"
  safeConvert RebasedDeps = pure $ SqlString "deps"
  safeConvert RebasedMaster = pure $ SqlString "master"

instance Convertible SqlValue RebaseStatus where
  safeConvert (SqlByteString str) =
    safeConvert (SqlString $ UTF8.toString str)
  safeConvert (SqlString "none") = pure NoBase
  safeConvert (SqlString "deps") = pure RebasedDeps
  safeConvert (SqlString "master") = pure RebasedMaster
  safeConvert x = convError "Invalid RebaseStatus" x

-- | Build status of a MR
data BuildStatus
  = Runnable -- ^ No build started yet
  | Running -- ^ Build running
  | Passed -- ^ Build has passed
  | FailedRebase -- ^ Failed to rebase
  | FailedBuild -- ^ Failed to build
  | FailedDeps -- ^ A dependency failed to build
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

isFailedStatus :: BuildStatus -> Bool
isFailedStatus FailedRebase = True
isFailedStatus FailedBuild = True
isFailedStatus FailedDeps = True
isFailedStatus _ = False

isPassedStatus :: BuildStatus -> Bool
isPassedStatus Passed = True
isPassedStatus _ = False

isPendingStatus :: BuildStatus -> Bool
isPendingStatus Running = True
isPendingStatus Runnable = True
isPendingStatus _ = False

-- | Combine a build status with a parent build status. The effective build
-- status will be the failure if the own status is a failure, 'FailedDeps' if
-- the parent status is a failure, and the own status otherwise - though
-- never anything decisive while the parent status is pending.
applyParentStatus :: BuildStatus -> BuildStatus -> BuildStatus
applyParentStatus parent self
  -- if self failed, use that failure
  | isFailedStatus self = self
  -- if parent failed, report that
  | isFailedStatus parent = FailedDeps
  -- if parent is pending, report Runnable or Running
  | isPendingStatus parent = max Running self
  -- parent succeeded, self is decisive
  | otherwise = self

instance Convertible BuildStatus SqlValue where
  safeConvert = safeConvert . show

instance Convertible SqlValue BuildStatus where
  safeConvert (SqlByteString str) =
    safeConvert (SqlString $ UTF8.toString str)
  safeConvert (SqlString str) =
    case readMaybe str of
      Nothing -> convError "Invalid BuildStatus" (SqlString str)
      Just status -> pure status
  safeConvert x = convError "Invalid BuildStatus" x

-- | Merge status of a MR
data MergeStatus
  = NotMerged
  | Merged
  | MergeFailed
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Convertible MergeStatus SqlValue where
  safeConvert NotMerged = pure $ SqlInt32 0
  safeConvert Merged = pure $ SqlInt32 1
  safeConvert MergeFailed = pure $ SqlInt32 (-1)

instance Convertible SqlValue MergeStatus where
  safeConvert sql = do
    i :: Integer <- safeConvert sql
    if i == 0 then
      pure NotMerged
    else if i < 0 then
      pure MergeFailed
    else
      pure Merged

makeSqlRow ''MergeRequest
