{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.PkgGroup
  ( PkgGroup (..),
    toPackageName,
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
import Data.Text (intercalate, isPrefixOf)
import HConf.Utils.Core (Name, PkgName (..))
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )

data PkgGroup = PkgGroup
  { name :: Name,
    dir :: Maybe FilePath,
    packages :: [Text],
    prefix :: Maybe Bool
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

instance ToJSON PkgGroup where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

toPackageName :: PkgGroup -> [PkgName]
toPackageName PkgGroup {..} = map pkgPath packages
  where
    pkgPath pkg =
      let pkgName = intercalate "-" ([name | fromMaybe False prefix] <> [pkg | pkg /= "."])
       in PkgName dir pkgName

--  in normalise (joinPath (maybeToList dir <> [unpack pkgName]))

isMember :: Name -> PkgGroup -> Bool
isMember pkgName = (`isPrefixOf` pkgName) . name
