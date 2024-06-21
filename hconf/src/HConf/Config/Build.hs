{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.Build
  ( Build (..),
    Builds,
    getBuild,
    getExtras,
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
import HConf.Core.PkgDir (PkgDir)
import HConf.Core.Version (Version, checkVersion)
import HConf.Utils.Class
  ( Check (..),
    FromConf (..),
    readPackages,
  )
import HConf.Utils.Core (Name, maybeMapToList, notElemError)
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )

type Extras = Map Text Version

data Build = Build
  { ghc :: Tag,
    resolver :: Text,
    extra :: Maybe Extras,
    include :: Maybe [Name],
    exclude :: Maybe [Name]
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

instance ToJSON Build where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance Check Build where
  check Build {..} =
    sequence_
      [ checkExtraDeps extra,
        checkPackageNames include,
        checkPackageNames exclude
      ]

checkPackageNames :: (FromConf m [PkgDir]) => Maybe [Name] -> m ()
checkPackageNames i = do
  known <- map toText <$> readPackages
  let unknown = fromMaybe [] i \\ known
  unless (null unknown) (fail ("unknown packages: " <> show unknown))

checkExtraDeps :: (MonadFail f, MonadIO f) => Maybe Extras -> f ()
checkExtraDeps = traverse_ checkVersion . maybeMapToList

type Builds = [Build]

getBuild :: (FromConf m Builds) => Tag -> m Build
getBuild v = do
  builds <- fromConf
  maybe (notElemError "build" (show v) (map ghc builds)) pure (find ((== v) . ghc) builds)

selectBuilds :: Tag -> [Build] -> [Build]
selectBuilds v = sortBy (\a b -> compare (ghc b) (ghc a)) . filter ((v <=) . ghc)

getExtras :: (FromConf m Builds) => Tag -> m [(Text, Version)]
getExtras tag =
  M.toList
    . M.fromList
    . concatMap (maybeMapToList . extra)
    . selectBuilds tag
    <$> fromConf
