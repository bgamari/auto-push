{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Jenkins
    ( Jenkins(..)
      -- * CSRF crumbs
    , requestCrumb
    , Crumb
      -- * Starting job builds
    , JobName(..)
    , startBuild
      -- * Querying build status
    , getBuildStatus
    , BuildStatus(..)
    , getConsoleText
    ) where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.List (intercalate)
import Data.String
import Control.Concurrent (threadDelay)

import qualified Data.CaseInsensitive as CI
import Control.Monad.Catch
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson
import Data.Aeson.Text
import Network.Wreq as Wreq
import Control.Lens

newtype JobName = JobName String
                deriving (IsString)

data Jenkins = Jenkins { baseUrl  :: String
                       , userName :: String
                       , token    :: String
                       }

newtype BuildUrl = BuildUrl String
                 deriving (Show, Eq, Ord, FromJSON, ToJSON)

type JobParams = [(String, String)]

--jenkinsBuild :: Jenkins
--             -> JobName
--             -> JobParams
--             -> BuildAction
--jenkinsBuild jobName params = do
--    startBuild jobName params

-- | A CSRF crumb
data Crumb = Crumb { crumb :: BS.ByteString
                   , crumbField :: BS.ByteString
                   }
           deriving (Show)

instance FromJSON Crumb where
    parseJSON = withObject "crumb" $ \o ->
        Crumb <$> fmap BS.pack (o .: "crumb")
              <*> fmap BS.pack (o .: "crumbRequestField")

requestCrumb :: Jenkins -> IO Crumb
requestCrumb j@Jenkins{..} = do
    resp <- Wreq.getWith opts (baseUrl <> "/crumbIssuer/api/json")
    view Wreq.responseBody <$> Wreq.asJSON resp
  where
    opts = Wreq.defaults & withAuth j

withCrumb :: Crumb -> Wreq.Options -> Wreq.Options
withCrumb (Crumb a b) = Wreq.header (CI.mk b) .~ [a]

withAuth :: Jenkins -> Wreq.Options -> Wreq.Options
withAuth Jenkins{..} = Wreq.auth .~ Just auth
  where auth = Wreq.basicAuth (BS.pack userName) (BS.pack token)

data QueueItemStatus = Queued | Started BuildUrl
                     deriving (Show)

-- | Returned by @/queue/item/N/api/json@.
instance FromJSON QueueItemStatus where
    parseJSON = withObject "queue item" $ \o -> started o <|> pure Queued
      where started o = do
                o' <- o .: "executable"
                Started <$> o' .: "url"

startBuild :: Jenkins -> Crumb -> JobName -> JobParams -> IO BuildUrl
startBuild j@Jenkins{..} crumb (JobName name) params = do
    resp <- Wreq.postWith (opts & Wreq.param "json" .~ [params']) url BS.empty
    when (resp ^. Wreq.responseStatus . Wreq.statusCode /= 201)
        $ fail $ "Failed to create build: "++views (Wreq.responseStatus . Wreq.statusCode) show resp
    queueItemUrl <- pure $ resp ^. Wreq.responseHeader "Location"
    buildUrl <- waitUntilStarted (BS.unpack queueItemUrl <> "api/json")
    return buildUrl
  where
    -- Now monitor the task until it has been started building.
    -- I wish this weren't so terrible but sadly Jenkins' API is awful; see
    -- Jenkins #31039
    waitUntilStarted queueItemUrl = do
        resp <- Wreq.getWith opts queueItemUrl
        status <- Wreq.asJSON resp
        case status ^. Wreq.responseBody of
          Queued -> threadDelay (1*1000*1000) >> waitUntilStarted queueItemUrl
          Started buildUrl -> return buildUrl

    params' = TL.toStrict $ encodeToLazyText params
    opts = Wreq.defaults & withAuth j . withCrumb crumb
    url = intercalate "/" [baseUrl, "job", name, "build"]

data BuildStatus = BuildSucceeded | BuildFailed | BuildUnstable | BuildRunning
                 deriving (Show)

-- | Returned by @/job/JOB/N/api/json@.
instance FromJSON BuildStatus where
    parseJSON = withObject "build status" $ \o -> do
        result <- o .:? "result"
        case result :: Maybe String of
          Just "SUCCESS"  -> return BuildSucceeded
          Just "UNSTABLE" -> return BuildUnstable
          Just "FAILURE"  -> return BuildFailed
          Just other      -> fail $ "Unknown build result "++other
          Nothing         -> return BuildRunning

getBuildStatus :: Jenkins -> Crumb -> BuildUrl -> IO BuildStatus
getBuildStatus j@Jenkins{..} crumb (BuildUrl buildUrl) = do
    resp <- Wreq.getWith opts (buildUrl <> "api/json")
    view Wreq.responseBody <$> Wreq.asJSON resp
  where
    opts = Wreq.defaults & withAuth j . withCrumb crumb

getConsoleText :: Jenkins -> Crumb -> BuildUrl -> IO BSL.ByteString
getConsoleText j crumb (BuildUrl buildUrl) =
    view Wreq.responseBody <$> Wreq.getWith opts (buildUrl <> "consoleText")
  where
    opts = Wreq.defaults & withAuth j . withCrumb crumb

