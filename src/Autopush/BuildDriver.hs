{-#LANGUAGE TemplateHaskell #-}
module Autopush.BuildDriver
where

import Git (GitRepo, Ref)
import Autopush.MergeRequest
import Control.Lens.TH

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
      }

makeLenses ''BuildDriver
