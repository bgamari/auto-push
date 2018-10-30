{-#LANGUAGE TemplateHaskell #-}
module Autopush.Run
where

import Autopush.Actions
import Autopush.MergeRequest
import Autopush.BuildDriver
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
import System.IO.Temp

data RunConfig
  = RunConfig
      { _numWorkers :: Int
      , _numWorkingCopies :: Int
      }
makeLenses ''RunConfig

defRunConfig :: RunConfig
defRunConfig =
  RunConfig
    { _numWorkers = 3
    , _numWorkingCopies = 3
    }

randomToken :: Int -> IO Text
randomToken n =
  Text.pack <$> replicateM n (randomRIO ('a','z'))

dummyBuildDriver :: BuildDriver
dummyBuildDriver = do
  BuildDriver
    { _buildStart = \repo ref -> do
        token <- randomToken 8
        return (BuildID token)
    , _buildStatus = \_ -> do
        return Passed
    , _buildCancel = \_ -> do
        return ()
    }

run :: GitRepo -> RunConfig -> IO ()
run repo config = do
  pool <- withSystemTempDirectory "autopush-test-git" $ \dir -> do
            mkWorkingCopies repo dir (config ^. numWorkingCopies)
  let driver = dummyBuildDriver
  replicateConcurrently_ (config ^. numWorkers) $ do
    workerID <- liftIO $ randomToken 8
    runAction repo pool driver $ do
      forever $ do
        runNextJob workerID
        liftIO $ do
          waitMilliseconds <- randomRIO (1000,2000)
          threadDelay (waitMilliseconds * 1000)
          return ()
