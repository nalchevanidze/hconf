{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.PkgGroup
  ( PkgGroup,
    PkgGroups,
    pkgDirs,
    isMember,
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
import HConf.Core.PkgDir (PkgDirs, genPkgDir)
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

pkgDirs :: PkgGroup -> PkgDirs
pkgDirs PkgGroup {..} = map pkgPath packages
  where
    pkgPath pkg = genPkgDir dir ([name | maybeBool prefix] <> [pkg | pkg /= "."])

isMember :: Name -> PkgGroups -> Bool
isMember pkgName = any ((`isPrefixOf` pkgName) . name)
