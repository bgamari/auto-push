module Hooks where

import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Servant.Client
import Server
import Git
import Utils
import PushMerge hiding (newMergeRequest)

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

preReceive :: IO ()
preReceive = do
    updates <- readReceiveRefs
    logMsg $ show updates
    return ()

postReceive :: IO ()
postReceive = do
    updates <- readReceiveRefs
    logMsg $ show updates
    -- Reset push branch back to former state
    updateRefs theRepo [ UpdateRef ref old Nothing
                       | (old, new, ref) <- updates
                       , Just _ <- pure $ isMergeBranch ref
                       ]
    logMsg $ show      [ UpdateRef ref old Nothing
                       | (old, new, ref) <- updates
                       , Just _ <- pure $ isMergeBranch ref
                       ]
    let postMergeRequest :: (SHA, SHA, Ref) -> ClientM ()
        postMergeRequest (old, new, ref) = do
            mergeReqId <- reqNewMergeRequest ref new
            case mergeReqId of
              Right reqId -> liftIO $ putStrLn $ show reqId
              Left BranchNotManaged -> liftIO $ putStrLn $ "Branch "++show ref++" is not managed"
              Left CommitHasNoMergeBase -> liftIO $ putStrLn $ "Commit "++show ref++" has no merge base with branch "++show ref
    request $ mapM_ postMergeRequest updates
    return ()
