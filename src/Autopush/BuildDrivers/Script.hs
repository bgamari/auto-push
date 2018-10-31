{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}

module Autopush.BuildDrivers.Script
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
import System.Exit
import Data.Maybe

data BuildJob = BuildJob BuildID GitRepo Ref

mkScriptBuildDriver :: Int -> FilePath -> IO BuildDriver
mkScriptBuildDriver numWorkers scriptFilename = do
  jobQueue <- newTChanIO
  jobStatus <- newTVarIO Map.empty
  jobCancelVars <- newTVarIO Set.empty

  let updateStatus :: BuildID -> BuildStatus -> IO ()
      updateStatus buildID s = do
        logMsg $ printf "update status for %s -> %s"
          (unBuildID buildID)
          (show s)
        atomically . modifyTVar jobStatus $ Map.insert buildID s

  replicateM numWorkers . forkIO $ do
    logMsg "started build worker"
    forever $ do
      BuildJob buildID repo ref <- atomically $ readTChan jobQueue
      logMsg $ printf "picked up build job %s" (unBuildID buildID)
      updateStatus buildID BuildPending
      let doTheJob = do
            logMsg $ printf "starting build: %s" (unBuildID buildID)
            withSystemTempDirectory "autopush-worker" $ \dir -> do
              catch
                (do
                  wrepo <- Git.clone repo dir
                  case Git.isBranch ref of
                    Nothing ->
                      error $ "Expected branch, but found " ++ Git.showRef ref
                    Just branch ->
                      Git.checkoutBranch
                        wrepo
                        branch
                        (Just (Git.Remote "origin", branch))
                  Git.resolveRef wrepo (Ref "HEAD") >>= logMsg . ("HEAD: " ++) . show
                  updateStatus buildID BuildRunning
                  logMsg $ printf "--- running: %s ---" (dir </> scriptFilename)
                  readFile (dir </> scriptFilename) >>= logMsg
                  (exitCode, stdoutData, stderrData) <-
                    readProcessWithExitCode (dir </> scriptFilename) [] ""
                  case exitCode of
                    ExitSuccess -> do
                      logMsg $ printf "build passed: %s" (unBuildID buildID)
                      logMsg $
                        printf
                          "--- stdout: ---\n%s\n--- stderr ---\n%s\n"
                          stdoutData
                          stderrData
                      updateStatus buildID BuildPassed
                    ExitFailure errno -> do
                      logMsg $ printf "build failed: %s" (unBuildID buildID)
                      let failMsg =
                            printf
                              "--- exit code: %i ---\n--- stdout: ---\n%s\n--- stderr ---\n%s\n"
                              errno
                              stdoutData
                              stderrData
                      logMsg $ failMsg
                      updateStatus buildID (BuildFailed failMsg)
                )
                (\(e :: SomeException) -> do
                    let failMsg = show e
                    logMsg $ failMsg
                    updateStatus buildID (BuildFailed failMsg)
                )
      let waitForCancellation = do
            atomically $ do
              cancelRequested <- Set.member buildID <$> readTVar jobCancelVars
              modifyTVar jobCancelVars $ Set.delete buildID
              if cancelRequested then
                return ()
              else
                retry
            logMsg $ printf "build cancellation requested: %s" (unBuildID buildID)
            updateStatus buildID BuildCancelled
      race_ doTheJob waitForCancellation

  return $ BuildDriver
    { _buildStart = \repo ref -> do
        buildID <- BuildID <$> randomToken 8
        logMsg $ printf "scheduling build: %s" (unBuildID buildID)
        let job = BuildJob buildID repo ref
        atomically $ writeTChan jobQueue job
        return buildID
    , _buildCancel = \buildID -> atomically $
        modifyTVar jobCancelVars $ Set.insert buildID
    , _buildStatus = \buildID -> atomically $ do
        fromMaybe BuildPending . Map.lookup buildID <$> readTVar jobStatus
    }
