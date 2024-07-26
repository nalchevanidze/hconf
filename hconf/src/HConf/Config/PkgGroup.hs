{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.PkgGroup
  ( PkgGroup ,
    PkgGroups,
    pkgDirs,
    isLocalPackage,
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
import Data.Text (isPrefixOf)
import HConf.Core.PkgDir (PkgDir, pkgDir)
import HConf.Utils.Class (ReadConf, readList)
import HConf.Utils.Core (Name, maybeBool)
import Relude hiding (isPrefixOf)

data PkgGroup = PkgGroup
  { name :: Name,
    dir :: Maybe FilePath,
    packages :: [Name],
    prefix :: Maybe Bool
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

type PkgGroups = [PkgGroup]

instance ToJSON PkgGroup where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

pkgDirs :: PkgGroup -> [PkgDir]
pkgDirs PkgGroup {..} = map pkgPath packages
  where
    pkgPath pkg = pkgDir dir ([name | maybeBool prefix] <> [pkg | pkg /= "."])

isLocalPackage :: (ReadConf m PkgGroups) => Name -> m Bool
isLocalPackage name = any (isMember name) <$> readList

isMember :: Name -> PkgGroup -> Bool
isMember pkgName = (`isPrefixOf` pkgName) . name
