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

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Control.Concurrent.Async
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
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
newtype ManagedBranch = ManagedBranch { getManagedBranch :: Ref }
                      deriving (Show, Eq, Ord, Generic)
                      deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype MergeRequestId = MergeRequestId Int
                       deriving (Eq, Ord, Show, Generic)
                       deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

-- | A request to merge some commits.
data MergeRequestState
    = MergeRequestState { _mergeRequestId   :: MergeRequestId
                        , _mergeOrigCommits :: CommitRange
                        , _mergeStatus      :: RequestStatus (Async BuildResult)
                        , _mergeBranch      :: ManagedBranch
                        }

-- | The status of a merge request.
data RequestStatus a
    = PendingBuild                     -- ^ needs a build to be started
    | FailedToRebase SHA               -- ^ failed to rebase onto the given base commit
    | Building CommitRange a           -- ^ building the given rebased commits
    | FailedToBuild CommitRange String -- ^ failed to build the given rebased commits
    | Succeeded CommitRange            -- ^ the given rebased commits were built sucessfully
    deriving (Show, Functor)

data BuildResult = BuildSucceeded
                 | BuildFailed String
                 deriving (Generic)
                 deriving anyclass (FromJSON, ToJSON)

-- | Test-build a SHA, returning 'Just' on error or 'Nothing' on success.
type BuildAction = SHA -> IO BuildResult


data BranchRequest a where
    NewMergeRequest :: { newMergeReqCommits :: CommitRange }
                    -> BranchRequest MergeRequestId
    CancelMergeRequest :: { cancelMergeReqId :: MergeRequestId }
                       -> BranchRequest ()
    GetBranchStatus :: BranchRequest BranchStatus

data BranchStatus = BranchStatus { branchCurrentHead :: SHA
                                 , branchMergeRequests :: [(MergeRequestId, RequestStatus ())]
                                 }

makeLenses ''MergeRequestState