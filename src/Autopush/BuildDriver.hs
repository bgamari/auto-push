{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Autopush.BuildDriver
where

import Git (GitRepo, Ref)
import Control.Lens.TH
import Data.Convertible
import Database.HDBC (SqlValue)
import Data.String
import qualified Data.Text as Text
import Data.Text (Text)

newtype BuildID = BuildID { unBuildID :: Text }
  deriving (Show, Eq, IsString, Ord)

instance Convertible SqlValue BuildID where
  safeConvert = fmap BuildID . safeConvert

instance Convertible BuildID SqlValue where
  safeConvert = safeConvert . unBuildID

data BuildStatus
  = BuildPending
  | BuildRunning
  | BuildCancelled
  | BuildFailed String
  | BuildPassed
  deriving (Show, Eq)

-- | Abstract interface for CI backends.
data BuildDriver
  = BuildDriver
      { _buildStart :: GitRepo -> Ref -> IO BuildID
        -- ^ Request a new build to be started. The returned 'BuildID' can be
        -- anything at the discretion of the CI backend, but it must uniquely
        -- identify the build.

      , _buildCancel :: BuildID -> IO ()
        -- ^ Cancel a running build, or dequeue a queued build.

      , _buildStatus :: BuildID -> IO BuildStatus
        -- ^ Get current build status.
      }

makeLenses ''BuildDriver
