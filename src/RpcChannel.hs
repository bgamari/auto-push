{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module RpcChannel where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Catch

data SomeRequest f where
    SomeRequest :: f a -> TMVar (Either SomeException a) -> SomeRequest f

data RpcChan f = RpcChan (TQueue (SomeRequest f))

newRpcChan :: STM (RpcChan f)
newRpcChan = RpcChan <$> newTQueue

handleRpc :: (MonadCatch m, MonadIO m)
          => RpcChan f -> (forall a. f a -> m a) -> m ()
handleRpc (RpcChan q) run = do
    SomeRequest req reply <- liftIO $ atomically $ readTQueue q
    res <- handle (pure . Left) $ fmap Right $ run req
    liftIO $ atomically $ putTMVar reply res

matchRpc :: RpcChan f -> (forall a. (Either SomeException a -> STM ()) -> f a -> b) -> STM b
matchRpc (RpcChan q) run = do
    SomeRequest req reply <- readTQueue q
    return $ run (putTMVar reply) req

sendRpc :: RpcChan f -> f a -> IO a
sendRpc (RpcChan q) req = do
    reply <- newEmptyTMVarIO
    atomically $ writeTQueue q (SomeRequest req reply)
    res <- atomically $ takeTMVar reply
    either throwM return res
