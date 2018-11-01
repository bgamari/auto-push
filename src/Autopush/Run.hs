{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Autopush.Run
where

import Autopush.Actions
import Autopush.MergeRequest
import Autopush.BuildDriver
import Autopush.BuildDrivers.Script
import Autopush.DB

import Git (GitRepo)
import System.Random
import qualified Data.Text as Text
import Data.Text (Text)
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Exception
import System.IO.Temp
import Utils

data RunConfig
  = RunConfig
      { _numWorkers :: Int
      , _numWorkingCopies :: Int
      , _numBuildWorkers :: Int
      }
makeLenses ''RunConfig

defRunConfig :: RunConfig
defRunConfig =
  RunConfig
    { _numWorkers = 1
    , _numWorkingCopies = 3
    , _numBuildWorkers = 2
    }

dummyBuildDriver :: BuildDriver
dummyBuildDriver = do
  BuildDriver
    { _buildStart = \repo ref -> do
        token <- randomToken 8
        return (BuildID token)
    , _buildStatus = \_ -> do
        return BuildPassed
    , _buildCancel = \_ -> do
        return ()
    }

run :: GitRepo -> RunConfig -> IO ()
run repo config = do
  withRepoDB repo . transactionally $ resetBuildInfo
  withSystemTempDirectory "autopush-test-git" $ \dir -> do
    pool <- mkWorkingCopies repo dir (config ^. numWorkingCopies)
    driver <- mkScriptBuildDriver (config ^. numBuildWorkers) ".autopush-build.sh"
    replicateConcurrently_ (config ^. numWorkers) $ do
      workerID <- liftIO $ randomToken 8
      forever $ do
        catch
          (runAction repo pool driver $ runNextJob workerID)
          (\(e :: SomeException) -> logMsg . show $ e)
        -- Randomize delay between runs a bit, to keep worker activity
        -- spread out
        liftIO $ do
          waitMilliseconds <- randomRIO (1000,2000)
          threadDelay (waitMilliseconds * 1000)
          return ()
