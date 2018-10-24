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
    , testCreateDependentMR
    , testUpdateMR
    , testCreateDependentMRSkip
    , testCreateDependentMRSkipOther
    , testGetActionableMR
    , testGetActionableMRFilter
    ]

withTempDB :: (SQLite.Connection -> IO a) -> IO a
withTempDB action =
  withSystemTempDirectory "autopush-test" $ \dir -> do
    initializeDB dir
    withDB dir action

testCreateMR :: TestTree
testCreateMR = testCase "create merge request" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      origBase = SHA "f572d396fae9206628714fb2ce00f72e94f2258f"
      origHead = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      expected =
        Just
          MergeRequest
            { mrID = 1
            , mrParent = Nothing
            , mrBuildStatus = Runnable
            , mrRebased = NoBase
            , mrBranch = branch
            , mrOriginalBase = origBase
            , mrOriginalHead = origHead
            , mrCurrentHead = origHead
            , mrMerged = NotMerged
            }
  actual <- createMergeRequest branch origBase origHead conn
  assertEqual "created MR" expected actual

testUpdateMR :: TestTree
testUpdateMR = testCase "update merge request" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      origBase = SHA "f572d396fae9206628714fb2ce00f72e94f2258f"
      origHead = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      otherHead = SHA "4cbd040533a2f43fc6691d773d510cda70f4126a"
      m =
        MergeRequest
          { mrID = 1
          , mrParent = Nothing
          , mrBuildStatus = Passed
          , mrRebased = RebasedMaster
          , mrBranch = branch
          , mrOriginalBase = origBase
          , mrOriginalHead = origHead
          , mrCurrentHead = otherHead
          , mrMerged = Merged
          }
      expected = Just m
  createMergeRequest branch origBase origHead conn
  updateMergeRequest m conn
  actual <- getMergeRequest 1 conn
  assertEqual "updated MR" expected actual

testCreateDependentMR :: TestTree
testCreateDependentMR = testCase "create dependent merge request" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      origBase = SHA "f572d396fae9206628714fb2ce00f72e94f2258f"
      origHead = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      otherHead = SHA "4cbd040533a2f43fc6691d773d510cda70f4126a"
      expected =
        Just
          MergeRequest
            { mrID = 2
            , mrParent = Just 1
            , mrBuildStatus = Runnable
            , mrRebased = NoBase
            , mrBranch = branch
            , mrOriginalBase = origBase
            , mrOriginalHead = origHead
            , mrCurrentHead = origHead
            , mrMerged = NotMerged
            }
  createMergeRequest (Branch "wip/world") origBase otherHead conn
  actual <- createMergeRequest branch origBase origHead conn
  assertEqual "created dependent MR" expected actual

testCreateDependentMRSkip :: TestTree
testCreateDependentMRSkip = testCase "create dependent merge request (skip one)" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      origBase = SHA "f572d396fae9206628714fb2ce00f72e94f2258f"
      origHead = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      otherHead = SHA "4cbd040533a2f43fc6691d773d510cda70f4126a"
      expected =
        Just
          MergeRequest
            { mrID = 3
            , mrParent = Just 1
            , mrBuildStatus = Runnable
            , mrRebased = NoBase
            , mrBranch = branch
            , mrOriginalBase = origBase
            , mrOriginalHead = origHead
            , mrCurrentHead = origHead
            , mrMerged = NotMerged
            }
  createMergeRequest (Branch "wip/world") origBase otherHead conn
  Just m <- createMergeRequest (Branch "wip/stuff") origBase otherHead conn
  updateMergeRequest m { mrMerged = Merged } conn
  actual <- createMergeRequest branch origBase origHead conn
  assertEqual "created dependent MR (skip merged)" expected actual

testCreateDependentMRSkipOther :: TestTree
testCreateDependentMRSkipOther = testCase "create dependent merge request (skip other)" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      origBase = SHA "f572d396fae9206628714fb2ce00f72e94f2258f"
      origHead = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      otherHead = SHA "4cbd040533a2f43fc6691d773d510cda70f4126a"
      expected =
        Just
          MergeRequest
            { mrID = 3
            , mrParent = Just 2
            , mrBuildStatus = Runnable
            , mrRebased = NoBase
            , mrBranch = branch
            , mrOriginalBase = origBase
            , mrOriginalHead = origHead
            , mrCurrentHead = origHead
            , mrMerged = NotMerged
            }
  Just m <- createMergeRequest (Branch "wip/world") origBase otherHead conn
  createMergeRequest (Branch "wip/stuff") origBase otherHead conn
  updateMergeRequest m { mrMerged = Merged } conn
  actual <- createMergeRequest branch origBase origHead conn
  assertEqual "created dependent MR (skip merged)" expected actual

testGetActionableMR :: TestTree
testGetActionableMR = testCase "get actionable MRs" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      origBase = SHA "f572d396fae9206628714fb2ce00f72e94f2258f"
      origHead = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      expected =
        [ MergeRequest
            { mrID = 1
            , mrParent = Nothing
            , mrBuildStatus = Runnable
            , mrRebased = NoBase
            , mrBranch = branch
            , mrOriginalBase = origBase
            , mrOriginalHead = origHead
            , mrCurrentHead = origHead
            , mrMerged = NotMerged
            }
        ]
  createMergeRequest branch origBase origHead conn
  actual <- getActionableMergeRequests conn
  assertEqual "actionable MRs" expected actual

testGetActionableMRFilter :: TestTree
testGetActionableMRFilter = testCase "get actionable MRs (skip non-actionable)" $ withTempDB $ \conn -> do
  let branch = Branch "wip/hello"
      otherBranch = Branch "wip/world"
      origBase = SHA "f572d396fae9206628714fb2ce00f72e94f2258f"
      origHead = SHA "9591818c07e900db7e1e0bc4b884c945e6a61b24"
      otherHead = SHA "4cbd040533a2f43fc6691d773d510cda70f4126a"
      expected =
        [ MergeRequest
            { mrID = 1
            , mrParent = Nothing
            , mrBuildStatus = Runnable
            , mrRebased = NoBase
            , mrBranch = branch
            , mrOriginalBase = origBase
            , mrOriginalHead = origHead
            , mrCurrentHead = origHead
            , mrMerged = NotMerged
            }
        ]
  createMergeRequest branch origBase origHead conn
  -- Create another MR, but mark it as merged - this shouldn't show up as
  -- actionable.
  Just m2 <- createMergeRequest otherBranch origBase otherHead conn
  updateMergeRequest m2 { mrMerged = Merged } conn
  actual <- getActionableMergeRequests conn
  assertEqual "actionable MRs" expected actual
