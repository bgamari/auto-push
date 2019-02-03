{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Common where

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


(.=?) :: ToJSON a => Text -> Maybe a -> Maybe Pair
key .=? Nothing = Nothing
key .=? Just x = Just $ key .= toJSON x

newtype UserId = UserId { getUserId :: Int }
               deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype Weight = Weight Int
               deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype AccessToken = AccessToken Text
                    deriving (Eq, Ord, Show, ToHttpApiData, IsString)

newtype MilestoneId = MilestoneId Int
                    deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype Sha = Sha { getSha :: Text }
            deriving (Eq, Ord, Show, ToHttpApiData, IsString, ToJSON, FromJSON)

newtype IssueLinkId = IssueLinkId Int
                    deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype ProjectId = ProjectId Int
                  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype PipelineId = PipelineId Int
                   deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype SnippetId = SnippetId { getSnippetId :: Int }
                  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype MergeRequestId = MergeRequestId Int
                       deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype MergeRequestIid = MergeRequestIid Int
                        deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype Labels = Labels (S.Set Text)
               deriving (Semigroup, Monoid, Show)

data StatusEvent = CloseEvent | ReopenEvent
                 deriving (Show)

instance ToJSON StatusEvent where
    toJSON CloseEvent  = "close"
    toJSON ReopenEvent = "reopen"

data Visibility = Private | Internal | Public
                deriving (Show)

instance ToJSON Visibility where
    toJSON Private  = "private"
    toJSON Internal = "internal"
    toJSON Public   = "public"

mkLabel :: Text -> Labels
mkLabel = Labels . S.singleton

instance IsString Labels where
    fromString = mkLabel . T.pack

instance ToJSON Labels where
    toJSON (Labels lbls) = toJSON $ T.intercalate "," (S.toList lbls)

newtype IssueIid = IssueIid { unIssueIid :: Int }
                 deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

type GitLabRoot = Header "Private-Token" AccessToken

type SudoParam = QueryParam "sudo" UserId

httpsBaseUrl :: String   -- ^ hostname
             -> BaseUrl
httpsBaseUrl hostname = BaseUrl Https hostname 443 "/api/v4"

httpBaseUrl :: String   -- ^ hostname
           -> BaseUrl
httpBaseUrl hostname = BaseUrl Http hostname 80 "/api/v4"

