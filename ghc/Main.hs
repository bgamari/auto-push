{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Data.Semigroup hiding (option)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T

import Options.Applicative
import Network.CircleCI.Build as Circle

import Git
import Server
import qualified PushMerge

opts :: Parser (AccountAPIToken, GitRepo)
opts =
    (,)
    <$> option (AccountAPIToken . T.pack <$> str) (long "token" <> short 't' <> help "CircleCI token")
    <*> argument (GitRepo <$> str) (help "Repository path" <> metavar "REPO")

main :: IO ()
main = do
    (token, srcRepo) <- execParser $ info (helper <*> opts) mempty
    let builder = circleCIBuilder token srcRepo
    runServer $ PushMerge.ServerConfig { PushMerge.repo = srcRepo
                                       , PushMerge.builder = builder
                                       , PushMerge.isMergeBranch = PushMerge.defaultIsMergeBranch
                                       }

destRepoOwner, destRepoName :: T.Text
destRepoOwner = "bgamari"
destRepoName = "test"

destRepo :: GitRepo
destRepo = GitRepo $ T.unpack $ "git@github.com:" <> destRepoOwner <> "/" <> destRepoName

circleCIBuilder :: Circle.AccountAPIToken
                -> GitRepo
                -> PushMerge.BuildAction
circleCIBuilder token srcRepo commit = bracket_ pushIt deleteIt $ do
    -- Trigger build
    let buildOpts = Circle.TriggerBuildOptions
                    { triggerBuildTarget = Circle.BuildRevision $ getSHA commit
                    , triggerBuildParams = mempty
                    }

    resp <- Circle.runCircleCI (Circle.triggerBuild projectPoint buildOpts) token
    case resp of
      Left err -> do
          print err
          return $ PushMerge.BuildFailed $ "Failed to trigger build: "++show err
      Right buildInfo -> do
          status <- waitUntilBuildFinishes token (Circle.number buildInfo)
          return status
  where
    -- Push to GitHub for testing
    pushIt =
        Git.push srcRepo destRemote (CommitSha commit) (branchRef branch)
    -- Clean up
    deleteIt =
        Git.push srcRepo destRemote (CommitRef $ Ref "") (branchRef branch)
    destRemote = Git.gitRepoToRemote destRepo
    branch = tempBranch commit

projectPoint :: Circle.ProjectPoint
projectPoint = Circle.ProjectPoint destRepoOwner destRepoName

tempBranch :: SHA -> Branch
tempBranch commit = Branch $ "auto-push/test-" <> getSHA commit

waitUntilBuildFinishes :: Circle.AccountAPIToken
                       -> Circle.BuildNumber
                       -> IO PushMerge.BuildResult
waitUntilBuildFinishes token buildNum = go
  where
    go = do
        threadDelay $ 30*1000*1000
        resp <- Circle.runCircleCI (Circle.getBuild projectPoint buildNum) token
        status <- case resp of
          Left err -> print err >> throwIO err
          Right status -> return status

        case Circle.outcome status of
          Nothing  -> go
          Just outcome -> return $ toResult outcome

    toResult outcome =
        case outcome of
          Circle.BuildSuccess  -> PushMerge.BuildSucceeded
          Circle.BuildFailed   -> PushMerge.BuildFailed "CircleCI build failed"
          Circle.BuildCanceled -> PushMerge.BuildFailed "CircleCI build canceled"
          Circle.BuildNoTests  -> PushMerge.BuildFailed "CircleCI build had no tests"
          Circle.BuildInfrastructureFail
                               -> PushMerge.BuildFailed "CircleCI infrastructure failure"
          Circle.BuildTimedOut -> PushMerge.BuildFailed "CircleCI build timed out"
