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
    ReadConf,
    readList,
  )
import HConf.Utils.Core
  ( Name,
    ResolverName,
    maybeList,
    maybeMapToList,
    notElemError,
    throwError,
  )
import Relude


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

instance (ReadConf m ()) => Check m Build where
  check Build {..} =
    sequence_
      [ checkExtraDeps extra,
        checkPkgNames include,
        checkPkgNames exclude
      ]

checkPkgNames :: (ReadConf m ()) => Maybe [PkgDir] -> m ()
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
  hkgRefs
    . M.fromList
    . concatMap (maybeMapToList . extra)
    . selectBuilds tag
    <$> readList

getPkgs :: (ReadConf m Builds) => Tag -> m [PkgDir]
getPkgs version = do
  Build {..} <- getBuild version
  pkgs <- readList
  pure ((pkgs <> maybeList include) \\ maybeList exclude)

getResolver :: (ReadConf m Builds) => Tag -> m ResolverName
getResolver version = resolver <$> getBuild version
