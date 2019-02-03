{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Autopush.Server where

import Control.Monad.IO.Class
import qualified Network.Wai.Handler.Warp as Warp
import Servant

import Git
import Autopush.MergeRequest
import Utils
import qualified Autopush.DB as DB
import Autopush.DB (withRepoDB)

type Port = Int

type Api =
          "merge"  :> Capture "merge" MergeRequestID
                   :> Get '[JSON] MergeRequest
     :<|> "merge"  :> Get '[JSON] [MergeRequestID]

server :: GitRepo -> Servant.Server Api
server repo =
         getMergeRequest
    :<|> listMergeRequests
  where
    listMergeRequests =
      liftIO . withRepoDB repo $ \conn -> do
        DB.listMergeRequests conn
    getMergeRequest reqId =
      maybe (throwError err404) pure =<<
        (liftIO . withRepoDB repo $ \conn -> do
          DB.getMergeRequest reqId conn)

-- reqGetMergeRequest :: MergeRequestID -> ClientM MergeRequest
-- reqListMergeRequests :: ClientM [MergeRequestID]
-- 
-- reqGetMergeRequest :<|> reqListMergeRequests
--     = client api
-- 
-- request :: ClientM a -> IO a
-- request action = do
--     mgr <- newManager defaultManagerSettings
--     let url = BaseUrl Http "localhost" serverPort ""
--     res <- runClientM action (ClientEnv mgr url Nothing)
--     case res of
--       Left err -> fail $ "Server.request: "++show err
--       Right a -> return a

api :: Proxy Api
api = Proxy

runServer :: Port -> GitRepo -> IO ()
runServer serverPort repo = do
    let app :: Application
        app = serve api (server repo)
    logMsg $ "Starting HTTP server on port " ++ show serverPort
    Warp.run serverPort app

