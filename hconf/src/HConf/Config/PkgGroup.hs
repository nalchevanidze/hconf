{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.PkgGroup
  ( PkgGroup (..),
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
import HConf.Core.PkgDir (PkgDir, pkgDir)
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

instance ToJSON PkgGroup where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

pkgDirs :: PkgGroup -> [PkgDir]
pkgDirs PkgGroup {..} = map pkgPath packages
  where
    pkgPath pkg = pkgDir dir ([name | maybeBool prefix] <> [pkg | pkg /= "."])

isMember :: Name -> PkgGroup -> Bool
isMember pkgName = (`isPrefixOf` pkgName) . name
