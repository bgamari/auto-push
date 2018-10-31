module Utils where

import System.IO
import Control.Monad
import System.Random
import Data.Text (Text)
import qualified Data.Text as Text

logMsg :: String -> IO ()
logMsg msg = putStrLn msg >> hFlush stdout

randomToken :: Int -> IO Text
randomToken n =
  Text.pack <$> replicateM n (randomRIO ('a','z'))

