module Main where

import Control.Concurrent
import System.Process

import Git
import PushMerge
import Server

main :: IO ()
main = do
    runServer $ ServerConfig { repo = GitRepo ".", builder = testBuilder }

-- | A simple builder for testing which fails depending upon whether the commit
-- message contains the string @fail@.
testBuilder :: BuildAction
testBuilder commit = do
    threadDelay (1000*1000)
    x <- readProcess "bash" [ "-c", "git show "++showSHA commit++" | grep fail | wc -l" ] ""
    putStrLn  $ "Build finished: "++show x
    return $ if read x > 0 then BuildFailed "failed" else BuildSucceeded
