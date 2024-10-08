{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.Config
  ( Config (..),
    getRule,
    nextRelease,
    updateConfig,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (toJSON),
    genericToJSON,
  )
import Data.Aeson.Types (defaultOptions)
import HConf.Config.Build (Builds)
import HConf.Config.PkgGroup (PkgGroup, isMember)
import HConf.Core.Bounds (Bounds, updateDepBounds, versionBounds)
import HConf.Core.Dependencies (Dependencies, getBounds, traverseDeps)
import HConf.Core.Version (Version, nextVersion)
import HConf.Utils.Class (Check (check), HConfIO, format)
import HConf.Utils.Core (DependencyName)
import HConf.Utils.FromConf (ReadConf)
import Relude

data Config = Config
  { version :: Version,
    bounds :: Bounds,
    groups :: [PkgGroup],
    builds :: Builds,
    dependencies :: Dependencies
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

getRule :: (MonadFail m) => DependencyName -> Config -> m Bounds
getRule name Config {..}
  | isMember (format name) groups = pure bounds
  | otherwise = getBounds name dependencies

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance (ReadConf m ()) => Check m Config where
  check Config {..} = traverse_ check (toList builds)

nextRelease :: Bool -> Config -> Config
nextRelease isBreaking Config {..} =
  let version' = nextVersion isBreaking version
      bounds' = versionBounds version'
   in Config {version = version', bounds = bounds', ..}

updateConfig :: (HConfIO m) => Config -> m Config
updateConfig Config {..} = do
  dependencies' <- traverseDeps updateDepBounds dependencies
  pure Config {dependencies = dependencies', ..}
