module Autopush.Hooks
( postReceive
, installHooks
)
where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Git
import Utils
import Autopush.MergeRequest
import Autopush.MergeBranch
import Autopush.DB
import System.Posix.Files
import Data.Bits ( (.|.) )
import System.FilePath

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

installHooks :: GitRepo -> IO ()
installHooks repo = do
    -- Create a database
    initializeRepoDB repo
    -- Install post-receive hook
    let hookFn = gitRepoDir repo </> "hooks" </> "post-receive"
    writeFile
      hookFn
      "#!/bin/sh\nauto-push post-receive"
    setFileMode
      hookFn
      ( ownerModes .|.
        groupReadMode .|. groupExecuteMode .|.
        otherReadMode .|. otherExecuteMode
      )

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

    let postMergeRequest :: (SHA, SHA, Ref) -> IO ()
        postMergeRequest (old, new, ref)
          | Just branch <- isBranch ref
          , Just mbranch <- isMergeBranch branch = do
              mr <- withRepoDB repo . transactionally $ \conn -> do
                -- Create a new merge request and add it to the job queue for
                -- scheduling.
                mr <- createMergeRequest mbranch new conn
                pushJob (mrID mr) conn
                return mr
              putStrLn $ "New merge request: " ++ show (mrID mr)
          | otherwise = return ()

    mapM_ postMergeRequest updates
    return ()
