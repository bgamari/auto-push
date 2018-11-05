{-#LANGUAGE ApplicativeDo #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Monoid
import Options.Applicative
import Data.Maybe
import qualified Data.Text as Text

import Git (cwdRepo, GitRepo (..))
import Autopush.Hooks
import Autopush.Run ( RunConfig (..)
                    , BuilderConfig (..)
                    , ScriptBuilderConfig (..)
                    , CircleCIBuilderConfig (..)
                    , defBuilderConfig, run
                    )
import Autopush.Server (runServer, Port)

data BuilderConfigType
  = ScriptBuilderConfigTy
  | CircleCIBuilderConfigTy
  deriving (Show, Eq)

data RawBuilderConfig
  = RawBuilderConfig
      { rbcType :: BuilderConfigType
      , rbcNumBuildWorkers :: Maybe Int
      , rbcBuildScript :: Maybe FilePath
      , rbcUsername :: Maybe String
      , rbcPassword :: Maybe String
      , rbcApiToken :: Maybe String
      , rbcGithubUsername :: Maybe String
      , rbcGithubProject :: Maybe String
      , rbcGithubPushRemote :: Maybe String
      }

modes :: Parser (IO ())
modes = subparser . mconcat . map wrap $ modeParsers
  where
    wrap (n, p) = command n (info (helper <*> p) fullDesc)
    modeParsers =
      [ ("post-receive", postReceiveMode)
      , ("install", installMode)
      , ("run", runMode)
      , ("api", apiMode)
      ]

printError :: IO () -> IO ()
printError = handle printExc
  where
    printExc :: SomeException -> IO ()
    printExc = print

postReceiveMode :: Parser (IO ())
postReceiveMode = pure . printError $ postReceive cwdRepo

targetRepo :: Parser GitRepo
targetRepo =
  GitRepo <$>
  strOption
    ( long "repo"
    <> short 'r'
    <> metavar "GIT_REPO"
    <> showDefault
    <> value "."
    <> help "git repository to manage"
    )

serverPort :: Parser Port
serverPort =
  option auto
    ( long "port"
    <> short 'p'
    <> metavar "SERVER_PORT"
    <> showDefault
    <> value 8880
    <> help "port to listen on for HTTP API"
    )


installMode :: Parser (IO ())
installMode =
  installHooks <$> targetRepo

numWorkers :: Parser Int
numWorkers =
  option auto
    ( long "workers"
    <> short 'w'
    <> metavar "NUM_WORKERS"
    <> showDefault
    <> value 3
    <> help "how many workers to spawn"
    )

numBuilders :: Parser Int
numBuilders =
  option auto
    ( long "builders"
    <> short 'w'
    <> metavar "NUM_BUILDERS"
    <> showDefault
    <> value 3
    <> help "how many builders to spawn"
    )

numWorkingCopies :: Parser Int
numWorkingCopies =
  option auto
    ( long "working-copies"
    <> short 'c'
    <> metavar "NUM_WORKING_COPIES"
    <> showDefault
    <> value 3
    <> help "how many working copies to clone"
    )

(??) :: a -> Maybe a -> a
(??) = fromMaybe

builderConfig :: Parser BuilderConfig
builderConfig =
  fromRawBuilder <$> rawBuilderConfig

rawBuilderConfigType :: Parser BuilderConfigType
rawBuilderConfigType =
  option (maybeReader parseRawBuilderConfigType)
    ( long "build-driver"
    <> short 'd'
    <> metavar "BUILD_DRIVER"
    <> showDefault
    <> value ScriptBuilderConfigTy
    <> help "which build driver to use for CI builds"
    )

builderScriptName :: Parser String
builderScriptName =
  option str
    ( long "build-script-name"
    <> metavar "SCRIPT"
    <> help "which shell script to run for CI builds"
    )

builderUsername :: Parser String
builderUsername =
  option str
    ( long "build-username"
    <> metavar "USERNAME"
    <> help "username to use for CI builds"
    )

builderPassword :: Parser String
builderPassword =
  option str
    ( long "build-password"
    <> metavar "PASSWORD"
    <> help "password to use for CI builds"
    )

builderApiToken :: Parser String
builderApiToken =
  option str
    ( long "build-api-token"
    <> metavar "API_TOKEN"
    <> help "API token to pass to build driver"
    )

builderGithubUsername :: Parser String
builderGithubUsername =
  option str
    ( long "github-user"
    <> metavar "USERNAME"
    <> help "GitHub user (used for external CI services)"
    )

builderGithubProject :: Parser String
builderGithubProject =
  option str
    ( long "github-project"
    <> metavar "PROJECT"
    <> help "GitHub project name (used for external CI services)"
    )

builderGithubPushRemote :: Parser String
builderGithubPushRemote =
  option str
    ( long "github-remote"
    <> metavar "REMOTE"
    <> help "Remote name under which the GitHub clone is known locally (used for external CI services)"
    )

parseRawBuilderConfigType :: String -> Maybe BuilderConfigType
parseRawBuilderConfigType "script" = Just ScriptBuilderConfigTy
parseRawBuilderConfigType "circleci" = Just CircleCIBuilderConfigTy
parseRawBuilderConfigType _ = Nothing

rawBuilderConfig :: Parser RawBuilderConfig
rawBuilderConfig =
  RawBuilderConfig <$> rawBuilderConfigType
                   <*> optional numBuilders
                   <*> optional builderScriptName
                   <*> optional builderUsername
                   <*> optional builderPassword
                   <*> optional builderApiToken
                   <*> optional builderGithubUsername
                   <*> optional builderGithubProject
                   <*> optional builderGithubPushRemote

fromRawBuilder :: RawBuilderConfig -> BuilderConfig
fromRawBuilder rb = case rbcType rb of
  ScriptBuilderConfigTy ->
    BuilderConfigScript $
      ScriptBuilderConfig
        (1 ?? rbcNumBuildWorkers rb)
        (".autopush-build" ?? rbcBuildScript rb)
  CircleCIBuilderConfigTy ->
    BuilderConfigCircleCI $
      CircleCIBuilderConfig
        { _cciUsername = Text.pack ("" ?? rbcGithubUsername rb)
        , _cciProject = Text.pack ("" ?? rbcGithubProject rb)
        , _cciToken = Text.pack ("" ?? rbcApiToken rb)
        , _cciPushRemote = Text.pack ("github" ?? rbcGithubPushRemote rb)
        }
  x ->
    error $ "Not implemented: builder type " ++ show x

runConfig :: Parser RunConfig
runConfig = 
  RunConfig <$> numWorkers
            <*> numWorkingCopies
            <*> builderConfig

runMode :: Parser (IO ())
runMode =
  run <$> targetRepo
      <*> runConfig

apiMode :: Parser (IO ())
apiMode = do
  runServer <$> serverPort
            <*> targetRepo

main :: IO ()
main = do
    join . fmap printError $ execParser $ info (helper <*> modes) mempty
