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
import HConf.Core.PkgDir (PkgDir)
import HConf.Core.Version (Version, printHackageRef)
import HConf.Utils.Class (FromConf (..))
import HConf.Utils.Core (Name, aesonYAMLOptions)
import HConf.Utils.Log (Log, task)
import HConf.Utils.Yaml (rewrite)
import Relude

data Stack = Stack
  { packages :: [PkgDir],
    resolver :: Name,
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

setupStack ::
  ( FromConf m Builds,
    FromConf m Env,
    FromConf m [PkgDir],
    Log m
  ) =>
  Tag ->
  m ()
setupStack version =
  task ("stack(" <> show version <> ")")
    $ task "stack.yaml"
    $ do
      p <- stack <$> fromConf
      rewrite p (updateStack version) $> ()

updateStack :: (FromConf m Builds, FromConf m [PkgDir]) => Tag -> Maybe Stack -> m Stack
updateStack version _ = do
  resolver <- getResolver version
  extraDeps <- sort . map printHackageRef <$> getExtras version
  packages <- getPkgs version
  pure
    Stack
      { allowNewer = Just (Latest == version),
        saveHackageCreds = Just False,
        ..
      }


