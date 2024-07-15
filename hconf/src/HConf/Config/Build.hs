{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.Build
  ( Build,
    Builds,
    getExtras,
    getPkgs,
    getResolver,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (toJSON),
    genericToJSON,
  )
import Data.Aeson.Types
  ( defaultOptions,
  )
import Data.List ((\\))
import qualified Data.Map as M
import HConf.Config.Tag (Tag)
import HConf.Core.HkgRef (HkgRef, hkgRefs)
import HConf.Core.PkgDir (PkgDir)
import HConf.Core.Version
  ( Version,
  )
import HConf.Utils.Class
  ( Check (..),
    FCon,
    HConfIO,
    fromConf,
    packages,
  )
import HConf.Utils.Core
  ( Name,
    ResolverName,
    maybeList,
    maybeMapToList,
    notElemError,
    throwError,
  )
import HConf.Utils.Log (Log (..))
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )

type Extras = Map Name Version

data Build = Build
  { ghc :: Tag,
    resolver :: ResolverName,
    extra :: Maybe Extras,
    include :: Maybe [PkgDir],
    exclude :: Maybe [PkgDir]
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

instance ToJSON Build where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance (HConfIO m, FCon m (), Log m) => Check m Build where
  check Build {..} =
    sequence_
      [ checkExtraDeps extra,
        checkPkgNames include,
        checkPkgNames exclude
      ]

checkPkgNames :: (FCon m (), Log m) => Maybe [PkgDir] -> m ()
checkPkgNames ls = do
  known <- packages
  let unknown = maybeList ls \\ known
  unless (null unknown) (throwError ("unknown packages: " <> show unknown))

checkExtraDeps :: (HConfIO m, FCon m (), Log m) => Maybe Extras -> m ()
checkExtraDeps = traverse_ check . maybe [] hkgRefs

type Builds = [Build]

getBuild :: (FCon m Builds) => Tag -> m Build
getBuild v = do
  builds <- fromConf
  maybe (notElemError "build" (show v) (map ghc builds)) pure (find ((== v) . ghc) builds)

selectBuilds :: Tag -> [Build] -> [Build]
selectBuilds v = sortBy (\a b -> compare (ghc b) (ghc a)) . filter ((v <=) . ghc)

getExtras :: (FCon m Builds) => Tag -> m [HkgRef]
getExtras tag =
  hkgRefs
    . M.fromList
    . concatMap (maybeMapToList . extra)
    . selectBuilds tag
    <$> fromConf

getPkgs :: (FCon m Builds) => Tag -> m [PkgDir]
getPkgs version = do
  Build {..} <- getBuild version
  pkgs <- packages
  pure ((pkgs <> maybeList include) \\ maybeList exclude)

getResolver :: (FCon m Builds) => Tag -> m ResolverName
getResolver version = resolver <$> getBuild version
