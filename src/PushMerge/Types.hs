{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module PushMerge.Types where

import GHC.Generics
import Data.Semigroup

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Control.Concurrent.Async
import Control.Lens
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Servant (ToHttpApiData, FromHttpApiData)

import Git

--------------------------------------------------
-- Queue
--------------------------------------------------

newtype Queue a = Queue (Seq.Seq a)
                deriving (Show, Functor, Foldable)

emptyQueue :: Queue a
emptyQueue = Queue mempty

popQueue :: Queue a -> Maybe (a, Queue a)
popQueue (Queue Seq.Empty)      = Nothing
popQueue (Queue (x Seq.:<| xs)) = Just (x, Queue xs)

appendQueue :: Queue a -> a -> Queue a
appendQueue (Queue xs) x = Queue $ xs Seq.:|> x

successors :: (Eq a) => a -> Queue a -> Maybe [a]
successors x (Queue xs) = go xs
  where
    go (y Seq.:<| ys)
      | x == y    = Just $ toList ys
      | otherwise = go ys
    go Seq.Empty  = Nothing



-- | A branch which we are responsible for merging into.
newtype ManagedBranch = ManagedBranch Branch
                      deriving (Show, Eq, Ord, Generic)
                      deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey,
                                        FromHttpApiData, ToHttpApiData)

isMergeBranch :: Branch -> Maybe ManagedBranch
isMergeBranch (Branch ref) = ManagedBranch . Branch <$> T.stripPrefix "merge/" ref

-- | The name of the branch which we pull merge requests from
mergeBranch :: ManagedBranch -> Branch
mergeBranch (ManagedBranch branch) = Branch $ "merge/" <> getBranchName branch

-- | The name of the branch which we are merging to
upstreamBranch :: ManagedBranch -> Branch
upstreamBranch (ManagedBranch branch) = branch



newtype MergeRequestId = MergeRequestId Int
                       deriving (Eq, Ord, Show, Generic)
                       deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

-- | A request to merge some commits.
data MergeRequestState
    = MergeRequestState { _mergeReqId          :: MergeRequestId
                        , _mergeReqOrigCommits :: CommitRange
                        , _mergeReqStatus      :: RequestStatus (Async BuildResult)
                        , _mergeReqBranch      :: ManagedBranch
                        }

-- | The status of a merge request.
data RequestStatus a
    = PendingBuild                     -- ^ needs a build to be started
    | FailedToRebase SHA               -- ^ failed to rebase onto the given base commit
    | Building CommitRange a           -- ^ building the given rebased commits
    | FailedToBuild CommitRange String -- ^ failed to build the given rebased commits
    | Succeeded CommitRange            -- ^ the given rebased commits were built sucessfully
    deriving (Show, Functor, Generic)
    deriving anyclass (FromJSON, ToJSON)

data BuildResult = BuildSucceeded
                 | BuildFailed String
                 deriving (Generic, Show)
                 deriving anyclass (FromJSON, ToJSON)

-- | Test-build a SHA, returning 'Just' on error or 'Nothing' on success.
type BuildAction = SHA -> IO BuildResult


data BranchRequest a where
    NewMergeRequest :: { newMergeReqHead :: SHA }
                    -> BranchRequest MergeRequestId
    CancelMergeRequest :: { cancelMergeReqId :: MergeRequestId }
                       -> BranchRequest ()
    GetBranchStatus :: BranchRequest BranchStatus

data BranchStatus = BranchStatus { branchCurrentHead :: SHA
                                 , branchMergeRequests :: [(MergeRequestId, RequestStatus ())]
                                 }
                  deriving (Show, Generic)
                  deriving anyclass (FromJSON, ToJSON)

makeLenses ''MergeRequestState
