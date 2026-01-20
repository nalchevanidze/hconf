{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Config.Config
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
import HMM.Config.Build (Builds)
import HMM.Config.Bump (Bump)
import HMM.Config.PkgGroup (PkgGroup, isMember)
import HMM.Core.Bounds (Bounds, updateDepBounds, versionBounds)
import HMM.Core.Dependencies (Dependencies, getBounds, traverseDeps)
import HMM.Core.Version (Version, nextVersion)
import HMM.Utils.Class (Check (check), HIO, format)
import HMM.Utils.Core (DependencyName)
import HMM.Utils.FromConf (ReadConf)
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

nextRelease :: Bump -> Config -> Config
nextRelease bump Config {..} =
  let version' = nextVersion bump version
      bounds' = versionBounds version'
   in Config {version = version', bounds = bounds', ..}

updateConfig :: (HIO m) => Config -> m Config
updateConfig Config {..} = do
  dependencies' <- traverseDeps updateDepBounds dependencies
  pure Config {dependencies = dependencies', ..}
