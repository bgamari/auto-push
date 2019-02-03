{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Pipeline 
  ( getPipeline
  , PipelineStatus(..)
  , PipelineResp(..)
  , cancelPipeline
  ) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import Data.Semigroup
import Data.Maybe
import Data.Aeson hiding (Success)
import Data.Aeson.Types (Parser, Pair)
import Data.Proxy
import Data.String
import Data.Time.Clock
import Servant.API
import Servant.Client
import GitLab.Common
import Control.Monad.IO.Class (liftIO)

----------------------------------------------------------------------
-- getPipeline
----------------------------------------------------------------------

type GetPipeline =
    GitLabRoot
    :> "projects"
    :> Capture "id" ProjectId
    :> "pipelines"
    :> Capture "pipeline_id" PipelineId
    :> Get '[JSON] PipelineResp

data PipelineStatus = Running | Pending | Success | Failed | Canceled | Skipped
                    deriving (Show)

instance FromJSON PipelineStatus where
  parseJSON o = parseJSON o >>= f
    where
      f :: T.Text -> Parser PipelineStatus
      f "running"  = pure Running
      f "pending"  = pure Pending
      f "success"  = pure Success
      f "failed"   = pure Failed
      f "canceled" = pure Canceled
      f "skipped"  = pure Skipped
      f other      = fail $ "Unknown pipeline status " ++ show other

data PipelineResp
    = PipelineResp { pipelineId :: PipelineId
                   , pipelineStatus  :: PipelineStatus
                   }
    deriving (Show)

instance FromJSON PipelineResp where
    parseJSON = withObject "pipeline response" $ \o ->
      PipelineResp
        <$> o .: "id"
        <*> o .: "status"

getPipeline :: AccessToken -> ProjectId -> PipelineId -> ClientM PipelineResp
getPipeline tok proj pipeline =
    client (Proxy :: Proxy GetPipeline) (Just tok) proj pipeline

----------------------------------------------------------------------
-- cancelPipeline
----------------------------------------------------------------------

type CancelPipeline =
    GitLabRoot
    :> "projects"
    :> Capture "id" ProjectId
    :> "pipelines"
    :> Capture "pipeline_id" PipelineId
    :> "cancel"
    :> Post '[JSON] PipelineResp

cancelPipeline :: AccessToken -> ProjectId -> PipelineId -> ClientM PipelineResp
cancelPipeline tok proj pipeline =
    client (Proxy :: Proxy CancelPipeline) (Just tok) proj pipeline

