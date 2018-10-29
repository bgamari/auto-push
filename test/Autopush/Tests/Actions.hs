{-#LANGUAGE OverloadedStrings #-}
module Autopush.Tests.Actions
( tests
)
where

import Autopush.Actions
import Autopush.DB
import Autopush.MergeRequest
import Autopush.MergeBranch

import Autopush.Tests.Helpers

import Test.Tasty
import Test.Tasty.HUnit

import System.Directory
import System.FilePath
import System.IO.Temp
import Control.Exception (bracket)
import Database.HDBC.Sqlite3 as SQLite
import Git (SHA (..), Branch (..), GitRepo (..), Commit (..), Ref (..))
import qualified Git
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import qualified Data.Text as Text
import qualified Database.HDBC as HDBC

tests :: TestTree
tests =
  testGroup "Actions"
    [ testPrepareMR
    , testScheduleMR1
    , testStartBuild
    , testCheckOnRunningBuild
    , testCheckOnFailingBuild
    ]

withClonedSRepo :: (GitRepo -> Action a) -> Action a
withClonedSRepo action = do
  withGitServer $ \srepo -> do
    let wcdir = takeDirectory (gitRepoDir srepo) </> "working"
    wc <- liftIO $ Git.clone srepo wcdir
    result <- action wc
    liftIO $ removePathForcibly wcdir
    return result

writeFileAndPush :: GitRepo -> FilePath -> String -> String -> Branch -> Ref -> IO ()
writeFileAndPush wrepo basename body commitMsg localBranch remoteRef = do
    let fn = gitRepoDir wrepo </> basename
    Git.runGit wrepo "checkout" [ "-b", Text.unpack $ getBranchName localBranch ] ""
    writeFile fn body
    Git.runGit wrepo "add" [ fn ] ""
    Git.runGit wrepo "commit" [ "-m", commitMsg ] ""
    Git.push
      wrepo
      originRemote
      (CommitRef $ Git.branchRef localBranch)
      remoteRef

testPrepareMR :: TestTree
testPrepareMR = testCase "prepare new MR" $
  runTestAction [] $ do
  withClonedSRepo $ \wrepo -> do
    withGitServer $ \srepo -> do
      liftIO $ do
        writeFileAndPush wrepo
          "hello" "hello\n"
          "initial commit"
          (Branch "master") (Ref "master")
        writeFileAndPush wrepo
          "hello" "hello again\n"
          "more hello"
          (Branch "hello2") (Ref "merge/master")
      db $ \conn -> do
        liftIO $ HDBC.quickQuery' conn "SELECT * FROM merge_requests" [] >>= print
      m1 <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      prepareMR (mrID m1)
      m2 <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      liftIO $ do
        assertEqual "same orig head" (mrOriginalHead m1) (mrOriginalHead m2)
        assertEqual "same curr head" (mrCurrentHead m1) (mrCurrentHead m2)
        assertEqual "same branch" (mrBranch m1) (mrBranch m2)
        assertEqual "pre-prepare base is empty" Nothing (mrOriginalBase m1)
        assertBool "post-prepare base is not empty" (isJust $ mrOriginalBase m2)

testScheduleMR1 :: TestTree
testScheduleMR1 = testCase "schedule one MR" $
  runTestAction [] $ do
  withClonedSRepo $ \wrepo -> do
    withGitServer $ \srepo -> do
      liftIO $ do
        writeFileAndPush wrepo
          "hello" "hello\n"
          "initial commit"
          (Branch "master") (Ref "master")
        writeFileAndPush wrepo
          "hello" "hello again\n"
          "more hello"
          (Branch "hello2") (Ref "merge/master")
      m1 <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      prepareMR (mrID m1)
      m2 <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      scheduleMR (mrID m2)
      m3 <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      liftIO $ do
        assertEqual "actually rebased" (mrOriginalHead m3) (mrCurrentHead m3)

testStartBuild :: TestTree
testStartBuild = testCase "start build" $
  runTestAction
    [ BuildStart (Ref "refs/heads/auto-push/to-build/1") "foobar"
    ] $ do
  withClonedSRepo $ \wrepo -> do
    withGitServer $ \srepo -> do
      liftIO $ do
        writeFileAndPush wrepo
          "hello" "hello\n"
          "initial commit"
          (Branch "master") (Ref "master")
        writeFileAndPush wrepo
          "hello" "hello again\n"
          "more hello"
          (Branch "hello2") (Ref "merge/master")
      prepareMR 1
      scheduleMR 1
      startBuild 1
      m <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      liftIO $ do
        assertEqual "status is 'Running'" Running (mrBuildStatus m)
        assertEqual "correct build ID stored" (Just "foobar") (mrBuildID m)

testCheckOnRunningBuild :: TestTree
testCheckOnRunningBuild = testCase "check running build" $
  runTestAction
    [ BuildStart (Ref "refs/heads/auto-push/to-build/1") "foobar"
    , BuildStatus "foobar" Running
    ] $ do
  withClonedSRepo $ \wrepo -> do
    withGitServer $ \srepo -> do
      liftIO $ do
        writeFileAndPush wrepo
          "hello" "hello\n"
          "initial commit"
          (Branch "master") (Ref "master")
        writeFileAndPush wrepo
          "hello" "hello again\n"
          "more hello"
          (Branch "hello2") (Ref "merge/master")
      prepareMR 1
      scheduleMR 1
      startBuild 1
      checkBuild 1
      m <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      liftIO $ do
        assertEqual "correct build ID stored" (Just "foobar") (mrBuildID m)
        assertEqual "status is 'Running'" Running (mrBuildStatus m)

testCheckOnFailingBuild :: TestTree
testCheckOnFailingBuild = testCase "check failing build" $
  runTestAction
    [ BuildStart (Ref "refs/heads/auto-push/to-build/1") "foobar"
    , BuildStatus "foobar" FailedBuild
    ] $ do
  withClonedSRepo $ \wrepo -> do
    withGitServer $ \srepo -> do
      liftIO $ do
        writeFileAndPush wrepo
          "hello" "hello\n"
          "initial commit"
          (Branch "master") (Ref "master")
        writeFileAndPush wrepo
          "hello" "hello again\n"
          "more hello"
          (Branch "hello2") (Ref "merge/master")
      prepareMR 1
      scheduleMR 1
      startBuild 1
      checkBuild 1
      m <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      liftIO $ do
        assertEqual "correct build ID stored" (Just "foobar") (mrBuildID m)
        assertEqual "status is 'Failed'" FailedBuild (mrBuildStatus m)
