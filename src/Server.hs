{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Server where

import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Aeson
import qualified Network.Wai.Handler.Warp
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Client

import Git
import qualified PushMerge
import PushMerge (MergeRequestId, BranchStatus, ManagedBranch)

serverPort :: Int
serverPort = 8880

-- | Is a branch managed?
data IsManaged a = Managed a
                 | NotManaged
                 deriving (Generic)
                 deriving anyclass (FromJSON, ToJSON)

type Api =
          "branch" :> Capture "branch" Branch
                   :> ReqBody '[JSON] SHA
                   :> Post '[JSON] (IsManaged MergeRequestId)
     :<|> "branch" :> Capture "branch" Branch
                   :> Get '[JSON] (IsManaged BranchStatus)
     :<|> "branch" :> Get '[JSON] [ManagedBranch]
     :<|> "merge"  :> Get '[JSON] [MergeRequestId]
     :<|> "merge"  :> Capture "merge" MergeRequestId :> Delete '[JSON] ()

server :: PushMerge.Server -> Servant.Server Api
server pmServer =
         newMergeRequest
    :<|> getBranchStatus
    :<|> listBranches
    :<|> listMergeRequests
    :<|> cancelMergeRequest
  where
    newMergeRequest ref sha =
        catchBranchNotManaged $ liftIO $ PushMerge.newMergeRequest pmServer ref sha
    getBranchStatus ref =
        catchBranchNotManaged $ liftIO $ PushMerge.getBranchStatus pmServer ref
    listBranches =
        liftIO $ PushMerge.listBranches pmServer
    listMergeRequests =
        undefined
    cancelMergeRequest reqId =
        liftIO $ PushMerge.cancelMergeRequest pmServer reqId

    catchBranchNotManaged =
        handle (\PushMerge.BranchNotManagedException -> return NotManaged) . fmap Managed

reqNewMergeRequest :: Branch -> SHA -> ClientM (IsManaged MergeRequestId)
reqGetBranchStatus :: Branch -> ClientM (IsManaged BranchStatus)
reqListBranches :: ClientM [ManagedBranch]
reqListMergeRequests :: ClientM [MergeRequestId]
reqCancelMergeRequest :: MergeRequestId -> ClientM ()
reqNewMergeRequest
    :<|> reqGetBranchStatus
    :<|> reqListBranches
    :<|> reqListMergeRequests
    :<|> reqCancelMergeRequest
    = client api

request :: ClientM a -> IO a
request action = do
    mgr <- newManager defaultManagerSettings
    let url = BaseUrl Http "localhost" serverPort ""
    res <- runClientM action (ClientEnv mgr url Nothing)
    case res of
      Left err -> fail $ "Server.request: "++show err
      Right a -> return a

api :: Proxy Api
api = Proxy

runServer :: PushMerge.ServerConfig -> IO ()
runServer config = do
    s <- PushMerge.startServer config
    let app :: Application
        app = serve api (server s)
    Network.Wai.Handler.Warp.run serverPort app
