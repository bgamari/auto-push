{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class
import Control.Monad.Catch
import qualified Network.Wai.Handler.Warp
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Client

import Git
import qualified PushMerge
import PushMerge (MergeRequestId, BranchStatus, ManagedBranch)

serverPort :: Int
serverPort = 8880

type Api =
          "branch" :> Capture "branch" Branch
                   :> ReqBody '[JSON] SHA
                   :> Post '[JSON] MergeRequestId
     :<|> "branch" :> Capture "branch" Branch
                   :> Get '[JSON] BranchStatus
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

    catchBranchNotManaged = handle (\PushMerge.BranchNotManagedException -> throwM err403)

reqNewMergeRequest :: Branch -> SHA -> ClientM MergeRequestId
reqGetBranchStatus :: Branch -> ClientM BranchStatus
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
