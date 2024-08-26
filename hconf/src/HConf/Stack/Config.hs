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
import HConf.Config.Build (Builds, getExtras, getPkgs, getResolver)
import HConf.Config.Tag (Tag (..))
import HConf.Core.Env (Env (..))
import HConf.Core.PkgDir (PkgDirs)
import HConf.Utils.Class (Format (..))
import HConf.Utils.Core (Name, aesonYAMLOptions, ResolverName)
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
setupStack version =
  task ("stack(" <> show version <> ")")
    $ task "stack.yaml"
    $ do
      p <- readEnv stack
      rewrite p (updateStack version) $> ()

updateStack :: (ReadConf m '[Builds, Env]) => Tag -> Maybe Stack -> m Stack
updateStack version _ = do
  resolver <- getResolver version
  extraDeps <- sort . map format <$> getExtras version
  packages <- getPkgs version
  pure
    Stack
      { allowNewer = Just (Latest == version),
        saveHackageCreds = Just False,
        ..
      }
