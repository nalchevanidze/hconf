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
import qualified Data.Map as M
import HConf.Config.Build (Build (..), Builds, getBuild, getExtras)
import HConf.Config.Tag (Tag (..))
import HConf.Core.Bounds (ReadBounds (readEnv))
import HConf.Core.Env (Env (..))
import HConf.Core.Version (Version)
import HConf.Utils.Class (FromConf (fromConf), ReadConf (..))
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

setupStack :: (FromConf m Builds, Log m, ReadBounds m) => Tag -> m ()
setupStack version = label ("stack(" <> show version <> ")") $ task "stack.yaml" $ do
  p <- stack <$> readEnv
  rewriteYaml p (updateStack version) $> ()

updateStack :: (FromConf m Builds) => Tag -> Stack -> m Stack
updateStack version _ = do
  Build {..} <- getBuild version
  builds <- fromConf
  pkgs <- readPackages
  let packages = (pkgs <> maybeList include) \\ maybeList exclude
  pure
    Stack
      { packages,
        resolver,
        allowNewer = Just (Latest == version),
        saveHackageCreds = Just False,
        extraDeps = sort $ map printExtra $ M.toList $ getExtras version builds
      }

printExtra :: (Text, Version) -> Text
printExtra (k, ver) = k <> "-" <> show ver
