{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.MergeRequest
    ( -- * Queries
      MergeRequestResp(..)
    , MergeRequestState(..)
    , getMergeRequestsByAssignee
      -- * Notes
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
    :> QueryParam "assignee_id" UserId
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
                       , mrIid :: MergeRequestIid
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
        <*> o .: "iid"
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
    client (Proxy :: Proxy GetMergeRequestsByAssignee) (Just tok) (Just uid)

----------------------------------------------------------------------
-- createMergeRequestNote
----------------------------------------------------------------------

type CreateMergeRequestNote =
    GitLabRoot
    :> SudoParam
    :> "projects"
    :> Capture "project_id" ProjectId
    :> "merge_requests"
    :> Capture "merge_request_id" MergeRequestIid
    :> "notes"
    :> ReqBody '[JSON] CreateNote
    :> Post '[JSON] CreateNoteResp

data CreateNote
    = CreateNote { cnBody :: T.Text }

instance ToJSON CreateNote where
    toJSON (CreateNote{..}) = object
        [ "body" .= cnBody ]

data CreateNoteResp = CreateNoteResp { cnId :: NoteId }

instance FromJSON CreateNoteResp where
    parseJSON = withObject "create note response" $ \o ->
      CreateNoteResp
        <$> o .: "id"

createMergeRequestNote :: AccessToken -> Maybe UserId -> ProjectId -> MergeRequestIid -> T.Text -> ClientM NoteId
createMergeRequestNote tok sudo proj mrid body = do
    cnId <$> client (Proxy :: Proxy CreateMergeRequestNote) (Just tok) sudo proj mrid (CreateNote body)

