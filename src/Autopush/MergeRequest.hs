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
import Data.String

import Autopush.MergeBranch

type MergeRequestID = Integer

type JobID = Integer

type WorkerID = Text

newtype BuildID = BuildID { unBuildID :: Text }
  deriving (Show, Eq, IsString)

instance Convertible SqlValue BuildID where
  safeConvert = fmap BuildID . safeConvert

instance Convertible BuildID SqlValue where
  safeConvert = safeConvert . unBuildID

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
      , mrBranch :: ManagedBranch
        -- ^ Branch name
      , mrOriginalBase :: Maybe SHA
        -- ^ SHA of the original base commit
      , mrOriginalHead :: SHA
        -- ^ SHA of the original branch head
      , mrCurrentHead :: SHA
        -- ^ SHA of the current (rebased) branch head
      , mrMerged :: MergeStatus
        -- ^ Whether this MR has been merged.
      , mrBuildID :: Maybe BuildID
        -- ^ Most recently started build, as reported by the CI driver
      }
      deriving (Show, Eq)

-- | What a MR is rebased onto.
data RebaseStatus
  = NotRebased
  | Rebased
  | RebaseFailed
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Convertible RebaseStatus SqlValue where
  safeConvert NotRebased = pure $ SqlInt32 0
  safeConvert Rebased = pure $ SqlInt32 1
  safeConvert RebaseFailed = pure $ SqlInt32 (-1)

instance Convertible SqlValue RebaseStatus where
  safeConvert sql = do
    i :: Integer <- safeConvert sql
    if i == 0 then
      pure NotRebased
    else if i < 0 then
      pure RebaseFailed
    else
      pure Rebased

-- | Build status of a MR
data BuildStatus
  = Runnable -- ^ No build started yet
  | Running -- ^ Build running (or queued)
  | Passed -- ^ Build has passed
  | FailedBuild -- ^ Failed to build
  | FailedDeps -- ^ A dependency failed to build
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

isFailedStatus :: BuildStatus -> Bool
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
