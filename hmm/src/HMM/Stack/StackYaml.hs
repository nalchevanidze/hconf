{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Stack.StackYaml
  ( Stack,
    syncStackYaml,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import HMM.Config.Build (Builds, getAllowNewer, getExtras, getPkgs, getResolver, resolveVersion)
import HMM.Config.Tag (Tag (..))
import HMM.Core.Env (Env (..))
import HMM.Core.PkgDir (PkgDirs)
import HMM.Utils.Class (Format (..))
import HMM.Utils.Core (Name, ResolverName, aesonYAMLOptions)
import HMM.Utils.FromConf (ReadConf, readEnv)
import HMM.Utils.Log (task)
import HMM.Utils.Yaml (rewrite_)
import Relude

data Stack = Stack
  { packages :: PkgDirs,
    resolver :: ResolverName,
    allowNewer :: Maybe Bool,
    saveHackageCreds :: Maybe Bool,
    extraDeps :: [Name]
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON Stack where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Stack where
  toJSON = genericToJSON aesonYAMLOptions

syncStackYaml :: (ReadConf m '[Builds, Env]) => Maybe Tag -> m ()
syncStackYaml tag = do
  version <- resolveVersion (fromMaybe Latest tag)
  task ("ghc-" <> show version <> "")
    $ task "stack.yaml"
    $ do
      p <- readEnv stack
      rewrite_ p (updateStack version)

updateStack :: (ReadConf m '[Builds, Env]) => Tag -> Maybe Stack -> m Stack
updateStack version _ = do
  resolver <- getResolver version
  extraDeps <- map format <$> getExtras version
  packages <- getPkgs version
  allowNewer <- getAllowNewer version
  pure
    Stack
      { saveHackageCreds = Just False,
        allowNewer = Just allowNewer,
        ..
      }
