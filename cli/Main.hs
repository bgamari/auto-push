module Main where

import Control.Exception
import Control.Monad
import Data.Monoid
import Options.Applicative

import Git (cwdRepo)
import Autopush.Hooks
import Autopush.Run

modes :: Parser (IO ())
modes =
  subparser
    $ command "post-receive" (info postReceiveMode fullDesc)
    <> command "install" (info installMode fullDesc)
    <> command "run" (info runMode fullDesc)

printError :: IO () -> IO ()
printError = handle printExc
  where
    printExc :: SomeException -> IO ()
    printExc = print

postReceiveMode :: Parser (IO ())
postReceiveMode = pure . printError $ postReceive cwdRepo

installMode :: Parser (IO ())
installMode = pure . printError $ installHooks cwdRepo

runMode :: Parser (IO ())
runMode = pure . printError $ run cwdRepo defRunConfig

main :: IO ()
main = do
    join $ execParser $ info (helper <*> modes) mempty
