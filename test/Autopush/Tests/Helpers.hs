{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Autopush.Tests.Helpers
where

import Test.Tasty
import Test.Tasty.HUnit
import Autopush.DB
import Autopush.MergeRequest
import Autopush.MergeBranch
import Autopush.Actions
import Autopush.Hooks
import Autopush.BuildDriver
import System.Directory
import System.IO.Temp
import Control.Exception (bracket)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Database.HDBC.Sqlite3 as SQLite
import Git (SHA (..), Branch (..), runGit, GitRepo (..) )
import qualified Git
import System.FilePath ( (</>) )
import Control.Monad
import System.Environment (getEnv)

data BuildChatLine
  = BuildStart Git.Ref BuildID
  | BuildCancel BuildID
  | BuildStatus BuildID BuildStatus
  deriving (Show)

chatBuildDriver :: GitRepo -> [BuildChatLine] -> IO (BuildDriver, IO [BuildChatLine])
chatBuildDriver expectedRepo chat = do
  chatVar <- newTVarIO chat
  let chatPopMay = atomically $ do
        readTVar chatVar >>= \case
          [] -> return Nothing
          (x:xs) -> writeTVar chatVar xs >> return (Just x)
      chatPop = maybe (error "End of chat script") return =<< chatPopMay
  
  let driver =
        BuildDriver
          { _buildStart = \repo ref -> do
              chatLine <- chatPop
              case chatLine of
                BuildStart expectedRef buildID -> do
                  assertEqual "buildStart repo" (gitRepoDir expectedRepo) (gitRepoDir repo)
                  assertEqual "buildStart ref" expectedRef ref
                  return buildID
                x -> do
                  assertFailure $ "Expected BuildStart " ++ show ref ++ ", but found " ++ show x
          , _buildCancel = \buildID -> do
              chatLine <- chatPop
              case chatLine of
                BuildCancel expectedBuildID -> do
                  assertEqual "buildCancel buildID" expectedBuildID buildID
                x -> do
                  assertFailure $ "Expected BuildCancel " ++ show buildID ++ ", but found " ++ show x
          , _buildStatus = \buildID -> do
              chatLine <- chatPop
              case chatLine of
                BuildStatus expectedBuildID status -> do
                  assertEqual "buildStatus buildID" expectedBuildID buildID
                  return status
                x -> do
                  assertFailure $ "Expected BuildStatus " ++ show buildID ++ ", but found " ++ show x
          }
  let peek = atomically $ readTVar chatVar
  return (driver, peek)

withTempDB :: (SQLite.Connection -> IO a) -> IO a
withTempDB action =
  withSystemTempDirectory "autopush-test-db" $ \dir -> do
    initializeDB dir
    withDB dir action

runTestAction :: [BuildChatLine] -> Action a -> IO a
runTestAction chat action = do
  -- home <- getEnv "HOME"
  -- let dir = home </> "tmp" </> "autopush-repos"
  -- do
  withSystemTempDirectory "autopush-test-git" $ \dir -> do
    let srepoDir = dir </> "managed.git"
        srepo = GitRepo srepoDir
    removePathForcibly srepoDir
    createDirectory srepoDir
    runGit srepo "init" [ "--bare", "." ] ""
    pool <- mkWorkingCopies srepo dir 3
    installHooks srepo
    (driver, peek) <- chatBuildDriver srepo chat

    retval <- runAction srepo pool driver action

    chatScriptRemainder <- peek
    assertBool
      ("Leftover chat script entries: " ++ show chatScriptRemainder)
      (null chatScriptRemainder)
    return retval
