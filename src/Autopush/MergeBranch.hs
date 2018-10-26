{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Autopush.MergeBranch
where

import Git (Branch (..))
import GHC.Generics
import Data.Semigroup
import qualified Data.Text as Text
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Data.Convertible (Convertible, safeConvert, convError)
import Database.HDBC (SqlValue)

-- | A branch which we are responsible for merging into.
newtype ManagedBranch =
  ManagedBranch
    { upstreamBranch :: Branch
      -- ^ The name of the branch we are merging to
    }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

isMergeBranch :: Branch -> Maybe ManagedBranch
isMergeBranch (Branch ref) = ManagedBranch . Branch <$> Text.stripPrefix "merge/" ref

-- | The name of the branch which we pull merge requests from
mergeBranch :: ManagedBranch -> Branch
mergeBranch (ManagedBranch branch) = Branch $ "merge/" <> getBranchName branch

instance Convertible ManagedBranch SqlValue where
  safeConvert = safeConvert . upstreamBranch

instance Convertible SqlValue ManagedBranch where
  safeConvert = fmap ManagedBranch . safeConvert

