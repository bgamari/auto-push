{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PushMerge where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import Data.Maybe
import GHC.Generics
import System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Map as M
import Servant (ToHttpApiData, FromHttpApiData)
import Data.Aeson

import Git
import Utils

theRepo = GitRepo "."

runBranch :: ManagedBranch -> IO ()
runBranch managedBranch = do
    return ()

isManagedBranch :: Ref -> Maybe ManagedBranch
isManagedBranch = Just . ManagedBranch

isMergeBranch :: Ref -> Maybe Ref
isMergeBranch (Ref ref) = Ref <$> T.stripPrefix "refs/heads/merge/" ref

data Event = NewMergeRequest ManagedBranch SHA
           deriving (Generic)
           deriving anyclass (FromJSON, ToJSON)

startServer :: IO Server
startServer = do
    serverBranches <- newTVarIO mempty
    serverNextRequestId <- newTVarIO (MergeRequestId 0)
    return $ Server {..}

data NewMergeRequestError = BranchNotManaged
                          | CommitHasNoMergeBase
                          deriving (Show, Generic)
                          deriving anyclass (FromJSON, ToJSON)

newMergeRequest :: Server -> Ref -> SHA
                -> IO (Either NewMergeRequestError MergeRequestId)
newMergeRequest server ref headSha
  | Just managed <- isManagedBranch ref = withBranchState server managed $ \state -> do
        baseSha <- resolveRef theRepo ref
        reqId <- freshRequestId server
        let commits = CommitRange baseSha headSha
            req = MergeRequest { mergeRequestId = reqId
                               , mergeOrigCommits = commits
                               , mergeCurCommits = commits
                               , mergeStatus = Building
                               , mergeBranch = managed
                               }
            state' = state { mergeQueue = mergeQueue state ++ [req] }
        return (state', Right reqId)
  | otherwise = return $ Left BranchNotManaged

withBranchState :: Server -> ManagedBranch
                -> (BranchState -> IO (BranchState, a))
                -> IO a
withBranchState server managed action = do
    bracketOnError takeState putState $ \(stateVar, state0) -> do
        (state, res) <- action state0
        atomically $ putTMVar stateVar state
        return res
  where
    branchesVar = serverBranches server

    takeState :: IO (TMVar BranchState, BranchState)
    takeState = atomically $ do
        branches <- readTVar branchesVar
        case M.lookup managed branches of
          Just stateVar -> do
              state <- takeTMVar stateVar
              return (stateVar, state)
          Nothing -> do
              stateVar <- newEmptyTMVar
              modifyTVar' branchesVar $ M.insert managed stateVar
              let state = BranchState managed []
              return (stateVar, state)
    putState :: (TMVar BranchState, BranchState) -> IO ()
    putState (stateVar, state0) = atomically $ putTMVar stateVar state0

freshRequestId :: Server -> IO MergeRequestId
freshRequestId server = atomically $ do
    i <- readTVar $ serverNextRequestId server
    writeTVar (serverNextRequestId server) $ case i of MergeRequestId i -> MergeRequestId (i+1)
    return i

data Server = Server { serverBranches :: TVar (M.Map ManagedBranch (TMVar BranchState))
                     , serverNextRequestId :: TVar MergeRequestId
                     }

-- | A branch which we are responsible for merging into.
newtype ManagedBranch = ManagedBranch { getManagedBranch :: Ref }
                      deriving (Show, Eq, Ord, Generic)
                      deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

-- | The state of a branch which we are responsible for merging into.
data BranchState = BranchState { branch      :: ManagedBranch
                               , mergeQueue  :: [MergeRequest]
                               }
                 deriving (Show)

newtype MergeRequestId = MergeRequestId Int
                       deriving (Eq, Ord, Show, Generic)
                       deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

-- | A request to merge some commits.
data MergeRequest = MergeRequest { mergeRequestId   :: MergeRequestId
                                 , mergeOrigCommits :: CommitRange
                                 , mergeCurCommits  :: CommitRange
                                 , mergeStatus      :: RequestStatus
                                 , mergeBranch      :: ManagedBranch
                                 }
                  deriving (Show)

-- | The status of a merge request.
data RequestStatus = FailedToRebase
                   | Building
                   | BuildFailed
                   | Succeeded
                   deriving (Show)

-- | Test-build a SHA, returning 'Just' on error or 'Nothing' on success.
type BuildAction = SHA -> IO (Maybe String)
