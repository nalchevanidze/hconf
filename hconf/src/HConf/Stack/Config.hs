{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Stack.Config
  ( Stack,
    setupStack,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.List ((\\))
import HConf.Config.Build (Build (..), Builds, getBuild, getExtras, getPkgs)
import HConf.Config.Tag (Tag (..))
import HConf.Core.Env (Env (..))
import HConf.Core.PkgDir (PkgDir)
import HConf.Core.Version (Version)
import HConf.Utils.Class (FromConf (..), readPackages)
import HConf.Utils.Core (Name, aesonYAMLOptions, maybeList)
import HConf.Utils.Log (Log, label, task)
import HConf.Utils.Yaml (rewriteYaml)
import Relude

data Stack = Stack
  { packages :: [Name],
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
  label ("stack(" <> show version <> ")")
    $ task "stack.yaml"
    $ do
      p <- stack <$> fromConf
      rewriteYaml p (updateStack version) $> ()

updateStack :: (FromConf m Builds, FromConf m [PkgDir]) => Tag -> Stack -> m Stack
updateStack version _ = do
  build@Build {resolver} <- getBuild version
  extras <- getExtras version
  packages <- getPkgs build
  pure
    Stack
      { packages,
        resolver,
        allowNewer = Just (Latest == version),
        saveHackageCreds = Just False,
        extraDeps = sort $ map printExtra extras
      }

printExtra :: (Text, Version) -> Text
printExtra (k, ver) = k <> "-" <> show ver
