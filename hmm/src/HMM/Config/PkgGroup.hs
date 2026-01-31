{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Config.PkgGroup
  ( PkgGroup,
    PkgGroups,
    pkgDirs,
    isMember,
    pkgGroupName,
    pkgRegistry,
    PkgRegistry,
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
import qualified Data.Map as Map
import HMM.Core.PkgDir (PkgDirs, genPkgDir)
import qualified HMM.Core.PkgDir as P
import HMM.Utils.Core (DependencyName (DependencyName), Name, PkgName (..))
import Relude

data PkgGroup = PkgGroup
  { name :: Name,
    dir :: Maybe FilePath,
    packages :: [Name],
    prefix :: Maybe Text
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

type PkgGroups = [PkgGroup]

instance ToJSON PkgGroup where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

pkgDirs :: PkgGroup -> PkgDirs
pkgDirs PkgGroup {..} = map pkgPath packages
  where
    pkgPath pkg = genPkgDir dir (maybeToList prefix <> [pkg | pkg /= "."])

isMember :: DependencyName -> PkgRegistry -> Bool
isMember (DependencyName name) = Map.member (PkgName name)

pkgGroupName :: PkgGroup -> Name
pkgGroupName PkgGroup {..} = name

resolveGroup :: PkgGroup -> IO PkgRegistry
resolveGroup g = Map.fromList . map ((,g) . P.name) <$> traverse P.resolvePkg (pkgDirs g)

type PkgRegistry = Map PkgName PkgGroup

pkgRegistry :: [PkgGroup] -> IO PkgRegistry
pkgRegistry = fmap Map.unions . traverse resolveGroup
