module Main where

import Control.Exception
import Control.Monad
import Data.Monoid
import Options.Applicative

import Git (cwdRepo)
import Autopush.Hooks
import Autopush.Run
import Autopush.Server (runServer)

modes :: Parser (IO ())
modes =
  subparser
    $ command "post-receive" (info postReceiveMode fullDesc)
    <> command "install" (info installMode fullDesc)
    <> command "run" (info runMode fullDesc)
    <> command "api" (info apiMode fullDesc)

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

apiMode :: Parser (IO ())
apiMode = pure . printError $ runServer cwdRepo

main :: IO ()
main = do
    join $ execParser $ info (helper <*> modes) mempty
