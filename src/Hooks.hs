module Hooks where

import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Servant.Client
import Server
import Git
import Utils
import PushMerge hiding (newMergeRequest, repo)

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

    -- Reset push branch back to former state
    let updates' = [ UpdateRef ref old Nothing
                   | (old, _new, ref) <- updates
                   , Just branch <- pure $ isBranch ref
                   , Just _ <- pure $ isMergeBranch branch
                   ]
    logMsg $ show updates'
    updateRefs repo updates'

    let postMergeRequest :: (SHA, SHA, Ref) -> ClientM ()
        postMergeRequest (_old, new, ref)
          | Just branch <- isBranch ref
          , Just _ <- isMergeBranch branch = do
            mergeReqId <- reqNewMergeRequest branch new
            case mergeReqId of
              reqId -> liftIO $ putStrLn $ "Reply: "++show reqId
              --Left BranchNotManaged -> liftIO $ putStrLn $ "Branch "++show ref++" is not managed"
              --Left CommitHasNoMergeBase -> liftIO $ putStrLn $ "Commit "++show ref++" has no merge base with branch "++show ref
          | otherwise = return ()

    request $ mapM_ postMergeRequest updates
    return ()
