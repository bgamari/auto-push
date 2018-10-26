{-#LANGUAGE OverloadedStrings #-}
module Autopush.Tests.Helpers
where

import Test.Tasty
import Test.Tasty.HUnit
import Autopush.DB
import Autopush.MergeRequest
import Autopush.MergeBranch
import Autopush.Actions
import Autopush.Hooks
import System.Directory
import System.IO.Temp
import Control.Exception (bracket)
import Control.Concurrent.STM
import Database.HDBC.Sqlite3 as SQLite
import Git (SHA (..), Branch (..), runGit, GitRepo (..) )
import qualified Git
import System.FilePath ( (</>) )
import Control.Monad

withTempDB :: (SQLite.Connection -> IO a) -> IO a
withTempDB action =
  withSystemTempDirectory "autopush-test-db" $ \dir -> do
    initializeDB dir
    withDB dir action

runTestAction :: Action a -> IO a
runTestAction action = do
  withSystemTempDirectory "autopush-test-git" $ \dir -> do
    let srepoDir = dir </> "managed.git"
        srepo = GitRepo srepoDir
    createDirectory srepoDir
    runGit srepo "init" [ "--bare", "." ] ""
    workingCopies <- forM [1..3] $ \i -> do
      Git.clone srepo (dir </> "wc-" ++ show i ++ ".git")
    pool <- newTVarIO workingCopies
    installHooks srepo

    runAction srepo pool action
