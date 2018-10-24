{-#LANGUAGE OverloadedStrings #-}
module Autopush.Tests.DB
( tests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Autopush.DB
import Autopush.MergeRequest
import System.Directory
import System.IO.Temp
import Control.Exception (bracket)
import Database.HDBC.Sqlite3 as SQLite
import Git (SHA (..), Branch (..) )

tests :: TestTree
tests =
  testGroup "DB"
    [ testCreateMR
    , testUpdateMR
    , testGetActionableMR
    , testGetActionableMRFilter
    , testCreateDependentMR
    , testCreateDependentMRSkip
    , testCreateDependentMRSkipOther
    ]

withTempDB :: (SQLite.Connection -> IO a) -> IO a
withTempDB action =
  withSystemTempDirectory "autopush-test" $ \dir -> do
    initializeDB dir
    withDB dir action

testCreateMR :: TestTree
testCreateMR = testCase "create merge request" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      head1 = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      expected =
        Just
          MergeRequest
            { mrID = 1
            , mrParent = Nothing
            , mrBuildStatus = Runnable
            , mrRebased = NotRebased
            , mrBranch = branch
            , mrOriginalHead = head1
            , mrCurrentHead = head1
            , mrMerged = NotMerged
            }
  actual <- createMergeRequest branch head1 conn
  assertEqual "created MR" expected actual

testUpdateMR :: TestTree
testUpdateMR = testCase "update merge request" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      head1 = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      head2 = SHA "4cbd040533a2f43fc6691d773d510cda70f4126a"
      m =
        MergeRequest
          { mrID = 1
          , mrParent = Nothing
          , mrBuildStatus = Passed
          , mrRebased = Rebased
          , mrBranch = branch
          , mrOriginalHead = head1
          , mrCurrentHead = head2
          , mrMerged = Merged
          }
      expected = Just m
  createMergeRequest branch head1 conn
  updateMergeRequest m conn
  actual <- getMergeRequest 1 conn
  assertEqual "updated MR" expected actual

testCreateDependentMR :: TestTree
testCreateDependentMR = testCase "create dependent merge request" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      head1 = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      head2 = SHA "4cbd040533a2f43fc6691d773d510cda70f4126a"
      expected =
        MergeRequest
          { mrID = 2
          , mrParent = Just 1
          , mrBuildStatus = Runnable
          , mrRebased = NotRebased
          , mrBranch = branch
          , mrOriginalHead = head1
          , mrCurrentHead = head1
          , mrMerged = NotMerged
          }
  Just m <- createMergeRequest (Branch "wip/world") head2 conn
  updateMergeRequest m { mrRebased = Rebased } conn
  Just m <- createMergeRequest branch head1 conn
  actual <- reparentMergeRequest m conn
  assertEqual "created dependent MR" expected actual

testCreateDependentMRSkip :: TestTree
testCreateDependentMRSkip = testCase "create dependent merge request (skip one)" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      head1 = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      head2 = SHA "4cbd040533a2f43fc6691d773d510cda70f4126a"
      head3 = SHA "f572d396fae9206628714fb2ce00f72e94f2258f"
      expected =
        MergeRequest
          { mrID = 3
          , mrParent = Just 1
          , mrBuildStatus = Runnable
          , mrRebased = NotRebased
          , mrBranch = branch
          , mrOriginalHead = head1
          , mrCurrentHead = head1
          , mrMerged = NotMerged
          }
  Just m <- createMergeRequest (Branch "wip/world") head2 conn
  updateMergeRequest m { mrRebased = Rebased } conn
  Just m <- createMergeRequest (Branch "wip/stuff") head3 conn
  updateMergeRequest m { mrMerged = Merged } conn
  Just m <- createMergeRequest branch head1 conn
  actual <- reparentMergeRequest m conn
  assertEqual "created dependent MR (skip merged)" expected actual

testCreateDependentMRSkipOther :: TestTree
testCreateDependentMRSkipOther = testCase "create dependent merge request (skip other)" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      head1 = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      head2 = SHA "4cbd040533a2f43fc6691d773d510cda70f4126a"
      head3 = SHA "f572d396fae9206628714fb2ce00f72e94f2258f"
      expected =
        MergeRequest
          { mrID = 3
          , mrParent = Just 2
          , mrBuildStatus = Runnable
          , mrRebased = NotRebased
          , mrBranch = branch
          , mrOriginalHead = head1
          , mrCurrentHead = head1
          , mrMerged = NotMerged
          }
  Just m <- createMergeRequest (Branch "wip/world") head2 conn
  updateMergeRequest m { mrMerged = Merged } conn
  Just m <- createMergeRequest (Branch "wip/stuff") head3 conn
  updateMergeRequest m { mrRebased = Rebased } conn
  Just m <- createMergeRequest branch head1 conn
  actual <- reparentMergeRequest m conn
  assertEqual "created dependent MR (skip merged)" expected actual

testGetActionableMR :: TestTree
testGetActionableMR = testCase "get actionable MRs" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      head1 = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      expected =
        [ MergeRequest
            { mrID = 1
            , mrParent = Nothing
            , mrBuildStatus = Runnable
            , mrRebased = NotRebased
            , mrBranch = branch
            , mrOriginalHead = head1
            , mrCurrentHead = head1
            , mrMerged = NotMerged
            }
        ]
  createMergeRequest branch head1 conn
  actual <- getActionableMergeRequests conn
  assertEqual "actionable MRs" expected actual

testGetActionableMRFilter :: TestTree
testGetActionableMRFilter = testCase "get actionable MRs (skip non-actionable)" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      otherBranch = Branch "wip/world"
      head1 = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      head2 = SHA "4cbd040533a2f43fc6691d773d510cda70f4126a"
      expected =
        [ MergeRequest
            { mrID = 1
            , mrParent = Nothing
            , mrBuildStatus = Runnable
            , mrRebased = NotRebased
            , mrBranch = branch
            , mrOriginalHead = head1
            , mrCurrentHead = head1
            , mrMerged = NotMerged
            }
        ]
  createMergeRequest branch head1 conn
  -- Create another MR, but mark it as merged - this shouldn't show up as
  -- actionable.
  Just m2 <- createMergeRequest otherBranch head2 conn
  updateMergeRequest m2 { mrMerged = Merged } conn
  actual <- getActionableMergeRequests conn
  assertEqual "actionable MRs" expected actual
