{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.MergeRequests 
    ( getMergeRequestsByAssignee
    , MergeRequestResp(..)
    , MergeRequestState(..)
    , createMergeRequestNote
    ) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import Data.Semigroup
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Proxy
import Data.String
import Data.Time.Clock
import Servant.API
import Servant.Client
import GitLab.Common
import Control.Monad.IO.Class (liftIO)

----------------------------------------------------------------------
-- getMergeRequestsByAssignee
----------------------------------------------------------------------

type GetMergeRequestsByAssignee =
    GitLabRoot :> "merge_requests"
    :> QueryString "assignee_id" UserId
    :> Get '[JSON] [MergeRequestResp]

data MergeRequestState = Opened | Closed | Locked | Merged
                       deriving (Show, Eq)

instance FromJSON MergeRequestState where
  parseJSON o = f <$> parseJSON o
    where
      f :: T.Text -> MergeRequestState
      f "opened" = Opened
      f "closed" = Closed
      f "locked" = Locked
      f "merged" = Merged

data MergeRequestResp
    = MergeRequestResp { mrId  :: MergeRequestId
                       , mrProjectId :: ProjectId
                       , mrState :: MergeRequestState
                       , mrWorkInProgress :: Bool
                       , mrSha :: Sha
                       , mrTargetBranch :: Text
                       , mrSourceBranch :: Text
                       , mrMergeWhenPipelineSucceeds :: Bool
                       , mrSquash :: Bool
                       }
    deriving (Show)

instance FromJSON MergeRequestResp where
    parseJSON = withObject "merge request response" $ \o ->
      MergeRequestResp
        <$> o .: "id"
        <*> o .: "project_id"
        <*> o .: "state"
        <*> o .: "work_in_progress"
        <*> o .: "sha"
        <*> o .: "target_branch"
        <*> o .: "source_branch"
        <*> o .: "merge_when_pipeline_succeeds"
        <*> o .: "squash"

getMergeRequestsByAssignee :: AccessToken -> UserId -> ClientM [MergeRequestResp]
getMergeRequestsByAssignee tok uid =
    client (Proxy :: Proxy GetMergeRequestsByAssignee) (Just tok) uid

----------------------------------------------------------------------
-- createMergeRequestNote
----------------------------------------------------------------------

type CreateMergeRequestNote =
    GitLabRoot :> "merge_requests"
    :> SudoParam
    :> Capture "merge_request_id" MergeRequestId
    :> "notes"
    :> ReqBody '[JSON] CreateNote
    :> Post '[JSON] ()

data CreateNote
    = CreateNote { cnBody :: T.Text }

instance ToJSON CreateNote where
    toJSON (CreateNote{..}) = object
        [ "body" .= cnBody ]

createMergeRequestNote :: AccessToken -> Maybe UserId -> MergeRequestId -> T.Text -> ClientM ()
createMergeRequestNote tok sudo mrid body = do
    client (Proxy :: Proxy CreateMergeRequestNote) (Just tok) sudo mrid (CreateNote body)

