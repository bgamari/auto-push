{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
  {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Commit where

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
-- getCommit
----------------------------------------------------------------------

type GetCommit =
    GitLabRoot
    :> "projects"
    :> Capture "id" ProjectId
    :> SudoParam
    :> "repository"
    :> "commits"
    :> Capture "sha" Sha
    :> Get '[JSON] CommitResp

data CommitResp
    = CommitResp { commitId  :: Sha
                 , commitLastPipeline :: PipelineId
                 }
    deriving (Show)

instance FromJSON CommitResp where
    parseJSON = withObject "commit response" $ \o ->
      CommitResp
        <$> o .: "id"
        <*> (o .: "last_pipeline" >>= (.: "id"))

getCommit :: AccessToken -> Maybe UserId -> ProjectId -> Sha -> ClientM CommitResp
getCommit tok sudo pid sha =
    client (Proxy :: Proxy GetCommit) (Just tok) pid sudo sha

