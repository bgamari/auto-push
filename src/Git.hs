{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Git
    ( -- * Identifying objects
      CommitRange(..)
    , Commit(..), showCommit
    , SHA(..), showSHA, zeroSHA
    , Ref(..), showRef
    , Branch(..), isBranch, branchRef
      -- * Repositories
    , cwdRepo
    , GitRepo(..)
    , clone
      -- * Querying repositories
    , resolveRef
    , mergeBase
      -- * Manipulating repositories
    , rebase
    , abortRebase
    , checkout
    , updateRefs
    , UpdateRefAction(..)
      -- * Remotes
    , Remote(..)
    , push
    , fetch
    , remoteUpdate
      -- * Exceptions
    , GitException(..)
    ) where

import Data.Semigroup
import Control.Monad
import GHC.Generics
import System.Exit
import Control.Exception
import Prelude hiding (head)

import qualified Data.Text as T
import System.Process

import qualified Network.URI as URI
import Data.Aeson
import Servant
import Utils

newtype GitRepo = GitRepo FilePath
                deriving (Show)

cwdRepo :: GitRepo
cwdRepo = GitRepo "."

data GitException = GitException { gitRepo     :: GitRepo
                                 , gitCommand  :: String
                                 , gitArgs     :: [String]
                                 , gitStderr   :: String
                                 , gitExitCode :: Int
                                 }
                  deriving (Show)
                  deriving anyclass (Exception)

-- | A range of commits which we can merge.
data CommitRange = CommitRange { baseCommit :: SHA
                               , headCommit :: SHA
                               }
                 deriving (Show, Generic)
instance ToJSON CommitRange
instance FromJSON CommitRange

-- | Run a @git@ command, throwing an exception on failure.
runGit :: GitRepo -> String -> [String] -> String -> IO String
runGit (GitRepo path) cmd args input = do
    logMsg $ "Git("++path++"): "++cmd++" "++unwords args++": "++input
    (code, out, err) <- readProcessWithExitCode "git" (["-C", path, cmd] ++ args) input
    case code of
      ExitSuccess -> return out
      ExitFailure n -> throwIO $ GitException { gitRepo = GitRepo path
                                              , gitCommand = cmd
                                              , gitArgs = args
                                              , gitStderr = err
                                              , gitExitCode = n
                                              }

resolveRef :: GitRepo -> Ref -> IO SHA
resolveRef repo ref =
    SHA . T.strip . T.pack <$> runGit repo "rev-parse" [showRef ref] ""

rebase :: GitRepo
       -> CommitRange  -- ^ range of commits to rebase (e.g. base..head)
       -> SHA          -- ^ onto
       -> IO CommitRange
rebase repo commits@(CommitRange base head) onto
  | base == onto = return commits
  | otherwise    = do
    void $ runGit repo "rebase" ["--onto", showSHA onto, showSHA base, showSHA head] ""
    CommitRange onto <$> resolveRef repo (Ref "HEAD")

abortRebase :: GitRepo -> IO ()
abortRebase repo = void $ runGit repo "rebase" ["--abort"] ""

checkout :: GitRepo
         -> Bool -- ^ force
         -> Commit
         -> IO ()
checkout repo force commit =
    void $ runGit repo "checkout" (if force then ["-f", ref] else [ref]) ""
  where
    ref = showCommit commit

mergeBase :: GitRepo -> Commit -> Commit -> IO SHA
mergeBase repo a b =
    SHA . T.strip . T.pack <$> runGit repo "merge-base" [showCommit a, showCommit b] ""

newtype SHA = SHA { getSHA :: T.Text }
            deriving (Show, Eq, Ord)
            deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)

newtype Ref = Ref { getRef :: T.Text }
            deriving (Show, Eq, Ord)
            deriving newtype (ToJSON, FromJSON)

instance FromHttpApiData Ref where
    parseUrlPiece = Right . Ref . T.pack . URI.unEscapeString . T.unpack

instance ToHttpApiData Ref where
    toUrlPiece (Ref x) = T.pack $ URI.escapeURIString URI.isUnescapedInURIComponent (T.unpack x)

newtype Branch = Branch { getBranchName :: T.Text }
            deriving (Show, Eq, Ord)
            deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

instance FromHttpApiData Branch where
    parseUrlPiece = Right . Branch . T.pack . URI.unEscapeString . T.unpack

instance ToHttpApiData Branch where
    toUrlPiece (Branch x) = T.pack $ URI.escapeURIString URI.isUnescapedInURIComponent (T.unpack x)

isBranch :: Ref -> Maybe Branch
isBranch (Ref b) = Branch <$> T.stripPrefix "refs/heads/" b

branchRef :: Branch -> Ref
branchRef (Branch b) = Ref $ "refs/heads/" <> b

data Commit = CommitSha SHA
            | CommitRef Ref

showCommit :: Commit -> String
showCommit (CommitSha sha) = showSHA sha
showCommit (CommitRef ref) = showRef ref

showSHA :: SHA -> String
showSHA (SHA ref) = T.unpack ref

showRef :: Ref -> String
showRef (Ref ref) = T.unpack ref

data UpdateRefAction = UpdateRef Ref SHA (Maybe SHA)
                     | CreateRef Ref SHA
                     | DeleteRef Ref (Maybe SHA)
                     deriving (Show)

zeroSHA :: SHA
zeroSHA = SHA $ T.replicate 40 (T.pack "0")

updateRefs :: GitRepo -> [UpdateRefAction] -> IO ()
updateRefs repo actions =
    void $ runGit repo "update-ref" ["--stdin"]
        $ unlines $ map toLine actions
  where
    sRef = T.unpack . getRef
    sSHA = T.unpack . getSHA
    toLine (UpdateRef ref new (Just old)) =
        unwords ["update", sRef ref, sSHA new, sSHA old]
    toLine (UpdateRef ref new Nothing) =
        unwords ["update", sRef ref, sSHA new]
    toLine (CreateRef ref new) =
        unwords ["create", sRef ref, sSHA new]
    toLine (DeleteRef ref (Just old)) =
        unwords ["delete", sRef ref, sSHA old]
    toLine (DeleteRef ref Nothing) =
        unwords ["delete", sRef ref]

newtype Remote = Remote T.Text

push :: GitRepo -> Remote -> Commit -> Ref -> IO ()
push repo (Remote remote) commit ref =
    void $ runGit repo "push" [ T.unpack remote
                              , showCommit commit ++ ":" ++ showRef ref
                              ] ""

clone :: GitRepo -> FilePath -> IO GitRepo
clone repo dest = do
    void $ runGit repo "clone" [".", dest] ""
    return dest'
  where dest' = GitRepo dest

fetch :: GitRepo -> Remote -> [Ref] -> IO ()
fetch repo (Remote remote) refs =
    void $ runGit repo "fetch" (T.unpack remote : map showRef refs) ""

remoteUpdate :: GitRepo -> Remote -> IO ()
remoteUpdate repo (Remote remote) =
    void $ runGit repo "remote" [ "update", T.unpack remote ] ""
