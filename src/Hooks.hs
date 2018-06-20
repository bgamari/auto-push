module Hooks where

import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Servant.Client
import Server
import Git
import Utils

-- | Read hook input provided by @git@ to a @post-receive@ or @pre-receive@
-- hook.
--
-- Returns tuples @(old, new, ref)@.
readReceiveRefs :: IO [(SHA, SHA, Ref)]
readReceiveRefs = mapMaybe parse . T.lines <$> T.getContents
  where
    parse line
      | [old, new, ref] <- T.words line
      = Just (SHA old, SHA new, Ref ref)
      | otherwise
      = Nothing

preReceive :: GitRepo -> IO ()
preReceive _ = do
    updates <- readReceiveRefs
    logMsg $ "pre-recieve: "++show updates
    return ()

postReceive :: GitRepo -> IO ()
postReceive repo = do
    updates <- readReceiveRefs
    logMsg $ show updates

    let postMergeRequest :: (SHA, SHA, Ref) -> ClientM ()
        postMergeRequest (old, new, ref)
          | Just branch <- isBranch ref = do
            mergeReqId <- reqNewMergeRequest branch new
            case mergeReqId of
              Managed reqId -> do
                  liftIO $ putStrLn $ "Reply: "++show reqId
                  -- Reset push branch back to former state
                  liftIO $ updateRefs repo [ UpdateRef ref old Nothing]
              NotManaged -> liftIO $ putStrLn $ "Branch "++show ref++" is not managed"
          | otherwise = return ()

    request $ mapM_ postMergeRequest updates
    return ()
