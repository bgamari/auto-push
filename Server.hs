{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class
import Control.Monad.Catch
import qualified Data.Map as M
import qualified Network.Wai.Handler.Warp
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Server
import Servant.Client

import Git
import PushMerge

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
server server =
         newMergeRequest
    :<|> getBranchStatus
    :<|> listBranches
    :<|> listMergeRequests
    :<|> cancelMergeRequest
  where
    newMergeRequest ref sha =
        catchBranchNotManaged $ liftIO $ PushMerge.newMergeRequest server ref sha
    getBranchStatus ref =
        catchBranchNotManaged $ liftIO $ PushMerge.getBranchStatus server ref
    listBranches =
        liftIO $ PushMerge.listBranches server
    listMergeRequests =
        undefined
    cancelMergeRequest reqId =
        liftIO $ PushMerge.cancelMergeRequest server reqId

    catchBranchNotManaged = handle (\BranchNotManagedException -> throwM err403)

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

runServer :: IO ()
runServer = do
    s <- startServer $ ServerConfig { repo = GitRepo ".", builder = testBuilder }
    let app :: Application
        app = serve api (server s)
    Network.Wai.Handler.Warp.run serverPort app
