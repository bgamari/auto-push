{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}

module Autopush.BuildDrivers.CircleCI
where

import Autopush.BuildDriver
import System.FilePath
import System.Process
import System.IO.Temp
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Utils
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Git (GitRepo, Ref (..))
import qualified Git
import Text.Printf
import Text.Read (readMaybe)
import System.Exit
import Data.Maybe
import qualified Network.CircleCI as CI
import qualified Network.CircleCI.Build as CI.Build
import qualified Data.Text as Text
import Data.Text (Text)

mkCircleCIBuildDriver :: Text -- ^ Github username
                      -> Text -- ^ Github project name
                      -> Git.Remote -- ^ Remote to push to
                      -> CI.Token -- ^ API token for CircleCI
                      -> IO BuildDriver
mkCircleCIBuildDriver username projectName pushRemote apiToken = do
  let project = CI.ProjectPoint username projectName
  return $ BuildDriver
    { _buildStart = \repo ref -> do
        -- First, let's figure out the right commit to push & build
        sha <- Git.resolveRef repo ref

        -- Push to whatever remote is configured for CircleCI
        Git.push repo pushRemote (Git.CommitSha sha) ref
        
        -- Now tell CircleCI to actually build it
        let tbo :: CI.Build.TriggerBuildOptions
            tbo = CI.Build.TriggerBuildOptions
                    (CI.Build.BuildRevision $ Git.getSHA sha)
                    []
        info <- either (error . show) pure =<<
                  CI.runCircleCI
                    (CI.Build.triggerBuild project tbo)
                    (CI.AccountAPIToken apiToken)
        return . BuildID . Text.pack . show $ CI.Build.number info

    , _buildCancel = \buildID -> do
        logMsg "Cancelling CircleCI builds not supported yet"
        
    , _buildStatus = \buildID -> do
        case readMaybe (Text.unpack $ unBuildID buildID) of
          Nothing ->
            return $ BuildFailed "Build ID is not a number"
          Just buildNumber -> do
            info <- either (error . show) pure =<<
                      CI.runCircleCI
                        (CI.Build.getBuild project $ CI.Build.BuildNumber buildNumber)
                        (CI.AccountAPIToken apiToken)
            case (CI.Build.lifecycle info, CI.Build.outcome info) of
              (CI.Build.BuildRunning, _) ->
                return BuildRunning
              (CI.Build.BuildFinished, Just CI.Build.BuildSuccess) ->
                return BuildPassed
              (CI.Build.BuildNotRun, _) ->
                return $ BuildFailed "build was not run"
              (CI.Build.BuildFinished, Just reason) ->
                return $ BuildFailed (show reason)
              (CI.Build.BuildFinished, Nothing) ->
                return $ BuildFailed "unknown reason"
              (_, _) ->
                return BuildPending
    }

