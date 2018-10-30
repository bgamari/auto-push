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
import System.Environment

tests :: TestTree
tests =
  testGroup "Actions"
    [ testPrepareMR
    , testScheduleMR1
    , testScheduleDependentMR
    , testStartBuild
    , testCheckOnRunningBuild
    , testCheckOnFailingBuild
    , testCheckOnFailingBuildWithDep
    , testHappyCase
    ]

defWorkerID :: WorkerID
defWorkerID = "def-test-worker"

withClonedSRepo :: (GitRepo -> Action a) -> Action a
withClonedSRepo action = do
  withGitServer $ \srepo -> do
    let wcdir = takeDirectory (gitRepoDir srepo) </> "working"
    -- home <- liftIO $ getEnv "HOME"
    -- let wcdir = home </> "tmp" </> "autopush-working"
    liftIO $ removePathForcibly wcdir
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

testScheduleDependentMR :: TestTree
testScheduleDependentMR = testCase "schedule dependent MR" $
  runTestAction [] $ do
  withClonedSRepo $ \wrepo -> do
    withGitServer $ \srepo -> do
      liftIO $ do
        writeFileAndPush wrepo
          "hello" "hello\n"
          "initial commit"
          (Branch "master") (Ref "master")
        writeFileAndPush wrepo
          "hello" "hello\nhello again\n"
          "more hello"
          (Branch "hello1") (Ref "merge/master")
        writeFileAndPush wrepo
          "hello" "hello\nhello again\n(once more)"
          "even more hello"
          (Branch "hello2") (Ref "merge/master")
      prepareMR 1
      scheduleMR 1
      m1 <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      prepareMR 2
      scheduleMR 2
      m1 <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 2 conn
      liftIO $ do
        -- TODO: assert things
        return ()

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
      -- prepareMR 1
      -- scheduleMR 1
      -- startBuild 1
      -- checkMR 1
      runNextJob defWorkerID
      runNextJob defWorkerID
      runNextJob defWorkerID
      runNextJob defWorkerID
      m <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      liftIO $ do
        assertEqual "correct build ID stored" (Just "foobar") (mrBuildID m)
        assertEqual "status is 'Running'" Running (mrBuildStatus m)

testHappyCase :: TestTree
testHappyCase = testCase "happy case" $
  runTestAction
    [ BuildStart (Ref "refs/heads/auto-push/to-build/1") "foobar"
    , BuildStatus "foobar" Passed
    ] $ do
  withGitServer $ \srepo -> do
    withClonedSRepo $ \wrepo -> do
      liftIO $ do
        writeFileAndPush wrepo
          "hello" "hello\n"
          "initial commit"
          (Branch "master") (Ref "master")
        writeFileAndPush wrepo
          "hello" "hello again\n"
          "more hello"
          (Branch "hello2") (Ref "merge/master")
    runAllJobs defWorkerID
    m <- db $ \conn -> do
      liftIO $ getExistingMergeRequest 1 conn
    liftIO $ do
      assertEqual "status is 'Passed'" Passed (mrBuildStatus m)
      assertEqual "merged" Merged (mrMerged m)
    withClonedSRepo $ \wrepo -> liftIO $ do
      contents <- readFile $ Git.gitRepoDir wrepo </> "hello"
      assertEqual "file has actually been merged" "hello again\n" contents

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
      -- 3 jobs should become available: prepare, schedule, start
      runNextJob defWorkerID
      runNextJob defWorkerID
      runNextJob defWorkerID
      m <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      liftIO $ do
        assertEqual "correct build ID stored" (Just "foobar") (mrBuildID m)
      runAllJobs defWorkerID
      m <- db $ \conn -> do
        liftIO $ getExistingMergeRequest 1 conn
      liftIO $ do
        assertEqual "build ID cleared" Nothing (mrBuildID m)
        assertEqual "status is 'Failed'" FailedBuild (mrBuildStatus m)

testCheckOnFailingBuildWithDep :: TestTree
testCheckOnFailingBuildWithDep = testCase "check failing build with dependency" $
  runTestAction
    [ BuildStart (Ref "refs/heads/auto-push/to-build/1") "foobar"
    , BuildStatus "foobar" Running
    , BuildStatus "foobar" Running
    , BuildStart (Ref "refs/heads/auto-push/to-build/2") "bazquux"
    , BuildStatus "foobar" FailedBuild
    , BuildCancel "bazquux"
    -- The following request doesn't actually happen, because the second MR is
    -- already "FailedDeps" by the time we get here
    -- , BuildStatus "bazquux" Passed
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
          (Branch "hello1") (Ref "merge/master")

      -- prepare 1
      runNextJob defWorkerID
      -- schedule 1
      runNextJob defWorkerID

      m <- db $ liftIO . getExistingMergeRequest 1
      liftIO $ assertEqual "MR1 rebased" Rebased (mrRebased m)

      liftIO $ do
        writeFileAndPush wrepo
          "hello" "hello once more\n"
          "even more hello"
          (Branch "hello2") (Ref "merge/master")

      runNextJob defWorkerID
      runNextJob defWorkerID
      runNextJob defWorkerID
      runNextJob defWorkerID
      runNextJob defWorkerID
      runNextJob defWorkerID

      (m1, m2) <- db $ \conn -> liftIO $ do
        (,) <$> getExistingMergeRequest 1 conn
            <*> getExistingMergeRequest 2 conn

      liftIO $ do
        assertEqual "MR2 parented onto MR1" (Just $ mrID m1) (mrParent m2)
        assertEqual "MR1: status is 'running'" Running (mrBuildStatus m1)
        assertEqual "MR2: status is 'running'" Running (mrBuildStatus m2)

      runAllJobs defWorkerID

      (m1, m2) <- db $ \conn -> liftIO $ do
        (,) <$> getExistingMergeRequest 1 conn
            <*> getExistingMergeRequest 2 conn

      liftIO $ do
        assertEqual "MR2 unparented" Nothing (mrParent m2)
        assertEqual "MR1: build ID cleared" Nothing (mrBuildID m1)
        assertEqual "MR2: build ID cleared" Nothing (mrBuildID m2)
        assertEqual "MR1: status is 'failed'" FailedBuild (mrBuildStatus m1)
        assertEqual "MR2: status is 'dependency failed'" FailedDeps (mrBuildStatus m2)

