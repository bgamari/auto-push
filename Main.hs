module Main where

import Control.Exception
import Data.Monoid
import Options.Applicative

import Git (cwdRepo)
import Hooks
import PushMerge
import Server

modes :: Parser (IO ())
modes = subparser
     $ command "pre-receive" (info preReceiveMode fullDesc)
    <> command "post-receive" (info postReceiveMode fullDesc)
    <> command "server" (info serverMode fullDesc)

printError :: IO () -> IO ()
printError = handle printExc
  where
    printExc :: SomeException -> IO ()
    printExc = print

preReceiveMode = pure $ printError $ preReceive cwdRepo
postReceiveMode = pure $ printError $ postReceive cwdRepo
serverMode = pure runServer

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) mempty
    mode

