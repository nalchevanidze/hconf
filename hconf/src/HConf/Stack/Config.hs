{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Stack.Config
  ( Stack,
    setupStack,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import HConf.Config.Build (Builds, getAllowNewer, getExtras, getPkgs, getResolver, resolveVersion)
import HConf.Config.Tag (Tag (..))
import HConf.Core.Env (Env (..))
import HConf.Core.PkgDir (PkgDirs)
import HConf.Utils.Class (Format (..))
import HConf.Utils.Core (Name, ResolverName, aesonYAMLOptions)
import HConf.Utils.FromConf (ReadConf, readEnv)
import HConf.Utils.Log (task)
import HConf.Utils.Yaml (rewrite)
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

setupStack :: (ReadConf m '[Builds, Env]) => Tag -> m ()
setupStack version = do
  v <- resolveVersion version
  task ("stack(" <> show v <> ")")
    $ task "stack.yaml"
    $ do
      p <- readEnv stack
      rewrite p (updateStack v) $> ()

updateStack :: (ReadConf m '[Builds, Env]) => Tag -> Maybe Stack -> m Stack
updateStack version _ = do
  resolver <- getResolver version
  extraDeps <- map format <$> getExtras version
  packages <- getPkgs version
  allowNewer <- getAllowNewer version
  pure
    Stack
      { saveHackageCreds = Just False,
        ..
      }
