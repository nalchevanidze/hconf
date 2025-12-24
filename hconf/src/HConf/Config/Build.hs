{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.Build
  ( Build,
    Builds,
    getExtras,
    getPkgs,
    getResolver,
    getAllowNewer,
    resolveVersion,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    genericParseJSON,
    genericToJSON,
  )
import Data.Foldable (Foldable (..))
import Data.List ((\\))
import qualified Data.Map as M
import HConf.Config.Tag (Tag (Latest))
import HConf.Core.HkgRef (HkgRef, VersionMap, hkgRefs)
import HConf.Core.PkgDir (PkgDirs)
import HConf.Utils.Class (Check (..), HConfIO (putLine))
import HConf.Utils.Core
  ( ResolverName,
    aesonYAMLOptions,
    maybeList,
    maybeMapToList,
    notElemError,
    throwError,
  )
import HConf.Utils.FromConf (ReadConf, readList)
import Relude

type Extras = VersionMap

data Build = Build
  { ghc :: Tag,
    resolver :: ResolverName,
    extra :: Maybe Extras,
    include :: Maybe PkgDirs,
    exclude :: Maybe PkgDirs,
    allowNewer :: Maybe Bool
  }
  deriving
    ( Generic,
      Show
    )

instance FromJSON Build where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Build where
  toJSON = genericToJSON aesonYAMLOptions

instance (ReadConf m ()) => Check m Build where
  check Build {..} =
    sequence_
      [ checkExtraDeps extra,
        checkPkgNames include,
        checkPkgNames exclude
      ]

checkPkgNames :: (ReadConf m ()) => Maybe PkgDirs -> m ()
checkPkgNames ls = do
  known <- readList
  let unknown = maybeList ls \\ known
  unless (null unknown) (throwError ("unknown packages: " <> show unknown))

checkExtraDeps :: (ReadConf m ()) => Maybe Extras -> m ()
checkExtraDeps = traverse_ check . maybe [] hkgRefs

type Builds = [Build]

getBuild :: (ReadConf m Builds) => Tag -> m Build
getBuild v = do
  builds <- readList
  maybe (notElemError "build" (show v) (map ghc builds)) pure (find ((== v) . ghc) builds)

selectBuilds :: Tag -> [Build] -> [Build]
selectBuilds v = sortBy (\a b -> compare (ghc b) (ghc a)) . filter ((v <=) . ghc)

getExtras :: (ReadConf m Builds) => Tag -> m [HkgRef]
getExtras tag =
  sort
    . hkgRefs
    . M.fromList
    . concatMap (maybeMapToList . extra)
    . selectBuilds tag
    <$> readList

getPkgs :: (ReadConf m Builds) => Tag -> m PkgDirs
getPkgs version = do
  Build {..} <- getBuild version
  pkgs <- readList
  pure ((pkgs <> maybeList include) \\ maybeList exclude)

getAllowNewer :: (ReadConf m Builds) => Tag -> m (Maybe Bool)
getAllowNewer version = allowNewer <$> getBuild version

resolveVersion :: (ReadConf m Builds) => Tag -> m Tag
resolveVersion Latest = do
  tags <- map ghc <$> readList
  pure $ maximum tags
resolveVersion v = pure v

getResolver :: (ReadConf m Builds) => Tag -> m ResolverName
getResolver version = resolver <$> getBuild version
