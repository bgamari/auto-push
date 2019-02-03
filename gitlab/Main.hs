{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Text as T

import qualified Database.HDBC.Sqlite3 as SQLite
import Servant.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Client as HTTP
import Options.Applicative

import GitLab.MergeRequest as GL
import GitLab.Common as GL
import GitLab.Commit as GL
import GitLab.Pipeline as GL
import GitLab.Project as GL

import Autopush.DB as A
import Autopush.MergeBranch as A
import Autopush.MergeRequest as A
import Autopush.BuildDriver
import qualified Git
import DB

data Config = Config { configGitlabToken :: AccessToken
                     , configBotUser :: UserId
                     , configProject :: ProjectId
                     , configHostname :: String
                     }

config :: Parser Config
config =
  Config <$> option (AccessToken <$> str)
                    (long "token" <> short 't' <> metavar "TOKEN" <> help "Gitlab access token")
         <*> option (UserId <$> auto)
                    (long "user" <> short 'u' <> metavar "USER-ID" <> help "Gitlab bot user ID")
         <*> option (ProjectId <$> auto)
                    (long "project" <> short 'p' <> metavar "PROJECT-ID" <> help "Project to monitor")
         <*> option str
                    (long "hostname" <> short 'H' <> metavar "HOSTNAME" <> help "GitLab host name")

main :: IO ()
main = do
    cfg <- execParser $ info (helper <*> config) mempty

    let repo :: Git.GitRepo
        repo = Git.GitRepo "."

    A.initializeRepoDB repo
    DB.initializeRepoDB repo
    withRepoDB repo $ \conn -> do
        mgr <- TLS.newTlsManager
        let env = Env { gitlabBaseUrl = GL.httpsBaseUrl (configHostname cfg)
                      , gitlabToken = configGitlabToken cfg
                      , gitlabUser = configBotUser cfg
                      , gitlabProject = configProject cfg
                      , gitRepo = repo
                      , httpManager = mgr
                      , dbConn = conn
                      }
        forever $ do poll env >> threadDelay (15*1000*1000)

data Env = Env { gitlabBaseUrl :: BaseUrl
               , gitlabToken   :: AccessToken
               , gitlabUser    :: UserId              -- ^ bot user
               , gitlabProject :: ProjectId
               , gitRepo       :: Git.GitRepo
               , dbConn        :: SQLite.Connection
               , httpManager   :: HTTP.Manager
               }

poll :: Env -> IO ()
poll env = handle onError $ do
    resp <- liftClientM env
            $ GL.getMergeRequestsByAssignee (gitlabToken env) (gitlabUser env)
    let interestingMRs :: [GL.MergeRequestResp]
        interestingMRs = [ mr
                         | mr <- resp
                         , not $ GL.mrWorkInProgress mr
                         , GL.mrProjectId mr == gitlabProject env
                         , GL.mrState mr == GL.Opened
                         ]
    mapM_ (handleMergeRequest env) interestingMRs
  where
    onError (SomeException e) = print e

handleMergeRequest :: Env -> GL.MergeRequestResp -> IO ()
handleMergeRequest env@(Env{..}) mr = do
    let sha = Git.SHA $ GL.getSha $ GL.mrSha mr
    mmr <- A.getMergeRequestByHead sha dbConn
    case mmr of
      Just mr' -> do
        -- Nothing to do
        Just glmr <- DB.getGitLabMergeRequest (A.mrID mr') dbConn
        if  | sha /= glmrOriginalHead glmr -> do
                leaveNote $ "Head commit changed, aborting merge."

            | DB.glmrLastCommentStatus glmr /= A.mrMergeRequestStatus mr' -> do
                leaveNote $ T.pack $ "Current state " ++ show (A.mrMergeRequestStatus mr')
                void $ DB.setLastCommentStatus (A.mrID mr') (A.mrMergeRequestStatus mr') dbConn

            | otherwise -> return ()

      Nothing
        | Just managedBranch <- isInterestingBranch $ GL.mrTargetBranch mr -> do
          projRemote <- liftClientM env $ projSshUrl <$> GL.getProject gitlabToken Nothing gitlabProject
          Git.fetch gitRepo (Git.Remote projRemote) [Git.Ref $ Git.getSHA sha] -- HACK

          -- Squash if desired
          sha' <- if mrSquash mr
                     then do
                       base <- Git.mergeBase gitRepo (Git.CommitSha sha)  
                                                     (Git.CommitRef $ Git.branchRef $ upstreamBranch managedBranch)
                       Git.squash gitRepo (Git.CommitSha base) (Git.CommitSha sha)
                     else return sha

          mr' <- A.createMergeRequest managedBranch sha' dbConn
          void $ DB.insertGitLabMergeRequest (A.mrID mr') sha (A.mrMergeRequestStatus mr') dbConn
          leaveNote $ "I've added this to my merge queue."
          return ()
        | otherwise -> return ()
  where
    leaveNote text =
      void $ liftClientM env $ GL.createMergeRequestNote gitlabToken Nothing (mrProjectId mr) (mrIid mr) text

isInterestingBranch :: T.Text -> Maybe ManagedBranch
isInterestingBranch branch
  | interesting branch = Just $ ManagedBranch $ Git.Branch branch
  | otherwise = Nothing
  where interesting "master" = True
        interesting _ = False

liftClientM :: Env -> ClientM a -> IO a
liftClientM Env{..} m = do
  result <- runClientM m (mkClientEnv httpManager gitlabBaseUrl)
  case result of
    Right r  -> pure r
    Left err -> fail $ "error: " ++ show err

gitlabBuildDriver :: Env -> BuildDriver
gitlabBuildDriver env@Env{..} =
    BuildDriver buildStart buildCancel buildStatus
  where
    buildStart repo ref = liftClientM env $ do
      sha <- liftIO $ Git.resolveRef repo ref
      projRemote <- projSshUrl <$> GL.getProject gitlabToken Nothing gitlabProject
      liftIO $ Git.push repo (Git.Remote projRemote) (Git.CommitSha sha) ref
      commit <- GL.getCommit gitlabToken Nothing gitlabProject (GL.Sha $ Git.getSHA sha)
      let pipelineId = commitLastPipeline commit
      return $ pipelineToBuildId $ pipelineId

    buildCancel bid = liftClientM env $ do
      let pipelineId = buildIdToPipeline bid
      void $ cancelPipeline gitlabToken gitlabProject pipelineId

    buildStatus bid = liftClientM env $ do
      let pipelineId = buildIdToPipeline bid
      pipeline <- getPipeline gitlabToken gitlabProject pipelineId
      let status = case GL.pipelineStatus pipeline of
                      GL.Pending  -> BuildPending
                      GL.Running  -> BuildRunning
                      GL.Success  -> BuildPassed
                      GL.Failed   -> BuildFailed "failed"
                      GL.Canceled -> BuildCancelled
                      GL.Skipped  -> BuildPassed
      pure status

pipelineToBuildId :: PipelineId -> BuildID
pipelineToBuildId (PipelineId n) = BuildID $ T.pack $ show n

buildIdToPipeline :: BuildID -> PipelineId
buildIdToPipeline = PipelineId . read . T.unpack . unBuildID

