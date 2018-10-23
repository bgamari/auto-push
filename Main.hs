module Main where

import Control.Exception
import Control.Monad
import Data.Monoid
import Options.Applicative

import Git (cwdRepo)
import Hooks

modes :: Parser (IO ())
modes = subparser
     $ command "pre-receive" (info preReceiveMode fullDesc)
    <> command "post-receive" (info postReceiveMode fullDesc)

printError :: IO () -> IO ()
printError = handle printExc
  where
    printExc :: SomeException -> IO ()
    printExc = print

preReceiveMode :: Parser (IO ())
preReceiveMode = pure $ printError $ preReceive cwdRepo

postReceiveMode :: Parser (IO ())
postReceiveMode = pure $ printError $ postReceive cwdRepo

main :: IO ()
main = do
    join $ execParser $ info (helper <*> modes) mempty
