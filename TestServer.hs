module Main where

import Control.Concurrent
import System.Process

import Git
import PushMerge
import Server

main :: IO ()
main = do
    runServer $ ServerConfig { repo = GitRepo "."
                             , builder = testBuilder
                             , isMergeBranch = defaultIsMergeBranch
                             }

-- | A simple builder for testing which fails depending upon whether the commit
-- message contains the string @fail@.
testBuilder :: BuildAction
testBuilder reqId commit = do
    putStrLn $ "Building "++show reqId
    threadDelay (1000*1000)
    x <- readProcess "bash" [ "-c", "git show "++showSHA commit++" | grep fail | wc -l" ] ""
    let n = read x :: Int
    putStrLn $ "Build finished: "++show x
    return $ if n > 0 then BuildFailed "failed" else BuildSucceeded
