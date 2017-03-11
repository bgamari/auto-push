{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class
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
         "branch" :> Capture "branch" Ref
                  :> ReqBody '[JSON] SHA
                  :> Post '[JSON] (Either NewMergeRequestError MergeRequestId)
     :<|> "merge" :> Capture "merge" MergeRequestId :> Delete '[JSON] ()

server :: PushMerge.Server -> Servant.Server Api
server server = postBranch :<|> merge
  where
    postBranch ref sha = liftIO $ PushMerge.newMergeRequest server ref sha
    merge = undefined

reqNewMergeRequest :: Ref -> SHA -> ClientM (Either NewMergeRequestError MergeRequestId)
reqMerge :: MergeRequestId -> ClientM ()
reqNewMergeRequest
    :<|> reqMerge
    = client api

request :: ClientM a -> IO a
request action = do
    mgr <- newManager defaultManagerSettings
    let url = BaseUrl Http "localhost" serverPort ""
    res <- runClientM action (ClientEnv mgr url)
    case res of
      Left err -> fail $ "Server.request: "++show err
      Right a -> return a

api :: Proxy Api
api = Proxy

runServer :: IO ()
runServer = do
    s <- startServer
    let app :: Application
        app = serve api (server s)
    Network.Wai.Handler.Warp.run serverPort app

