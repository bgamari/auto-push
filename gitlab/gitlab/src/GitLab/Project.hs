{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Project 
  ( -- * Queries
    ProjectResp(..)
  , getProject
    -- * Members
  , AccessLevel(..)
  , addProjectMember
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.ByteString as BS
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

----------------------------------------------------------------------
-- getProject
----------------------------------------------------------------------

data ProjectResp
    = ProjectResp { projId :: ProjectId
                  , projDescription :: Text
                  , projSshUrl :: Text
                  }
    deriving (Show)

instance FromJSON ProjectResp where
  parseJSON = withObject "project" $ \o -> 
    ProjectResp <$> o .: "id"
                <*> o .: "description"
                <*> o .: "ssh_url_to_repo"

type GetProjectAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId
    :> SudoParam
    :> Get '[JSON] ProjectResp

getProject :: AccessToken -> Maybe UserId -> ProjectId
           -> ClientM ProjectResp
getProject tok sudo prj = do
    client (Proxy :: Proxy GetProjectAPI) (Just tok) prj sudo

----------------------------------------------------------------------
-- addProjectMember
----------------------------------------------------------------------

type AddProjectMemberAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "members"
    :> ReqBody '[JSON] AddProjectMember
    :> Post '[JSON] Object

data AccessLevel = Guest | Reporter | Developer | Maintainer | Owner

instance ToJSON AccessLevel where
    toJSON l = toJSON $ case l of
                          Guest      -> 10 :: Int
                          Reporter   -> 20
                          Developer  -> 30
                          Maintainer -> 40
                          Owner      -> 50

data AddProjectMember = AddProjectMember UserId AccessLevel

instance ToJSON AddProjectMember where
    toJSON (AddProjectMember uid access) = object
        [ "user_id" .= uid
        , "access_level" .= access
        ]

addProjectMember :: AccessToken -> ProjectId
                 -> UserId -> AccessLevel -> ClientM ()
addProjectMember tok prj uid access = do
    client (Proxy :: Proxy AddProjectMemberAPI) (Just tok) prj
        $ AddProjectMember uid access
    return ()

