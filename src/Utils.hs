module Utils where

import System.IO

logMsg :: String -> IO ()
logMsg msg = putStrLn msg >> hFlush stdout
