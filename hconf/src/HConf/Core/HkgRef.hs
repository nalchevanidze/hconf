{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.HkgRef
  ( fetchVersions,
    hkgRefs,
    HkgRef,
    VersionMap,
  )
where

import Data.List.NonEmpty (toList)
import qualified Data.Map as M
import HConf.Core.Version (Version)
import HConf.Utils.Class
  ( Check (..),
    Format (..),
    HConfIO,
  )
import HConf.Utils.Core
  ( DependencyName,
    checkElem,
    getField,
  )
import HConf.Utils.Http (hackage)
import Relude hiding
  ( Undefined,
    break,
    drop,
    fromList,
    isPrefixOf,
    length,
    null,
    show,
    toList,
  )

type Versions = NonEmpty Version

type VersionMap = Map DependencyName Version

data HkgRef = HkgRef
  { name :: DependencyName,
    version :: Version
  }
  deriving (Eq, Ord)

fetchVersions :: (HConfIO m) => DependencyName -> m Versions
fetchVersions name = hackage ["package", format name, "preferred"] >>= getField "normal-version"

instance (HConfIO m) => Check m HkgRef where
  check HkgRef {..} = fetchVersions name >>= checkElem "version" (format name) version . toList

hkgRefs :: VersionMap -> [HkgRef]
hkgRefs = map (uncurry HkgRef) . M.toList

instance Format HkgRef where
  format HkgRef {..} = format name <> "-" <> format version
