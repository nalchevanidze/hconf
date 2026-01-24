{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Core.HkgRef
  ( fetchVersions,
    hkgRefs,
    HkgRef,
    VersionMap,
    VersionsMap,
    Versions,
  )
where

import Data.List.NonEmpty (toList)
import qualified Data.Map as M
import HMM.Core.Version (Version)
import HMM.Utils.Class
  ( Check (..),
    Format (..),
    HIO (..),
  )
import HMM.Utils.Core
  ( DependencyName,
    checkElem,
    getField,
  )
import HMM.Utils.Http (hackage)
import Relude (show)
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

type VersionsMap = M.Map DependencyName Versions

type VersionMap = Map DependencyName Version

data HkgRef = HkgRef
  { name :: DependencyName,
    version :: Version
  }
  deriving (Eq, Ord)

fetchVersions :: (HIO m) => DependencyName -> m Versions
fetchVersions name = do
  putLine ("Fetching Versions: " <> show name)
  hackage ["package", format name, "preferred"] >>= getField "normal-version"

instance (HIO m ) => Check m HkgRef where
  check HkgRef {..} = fetchVersions name >>= checkElem "version" (format name) version . toList

hkgRefs :: VersionMap -> [HkgRef]
hkgRefs = map (uncurry HkgRef) . M.toList

instance Format HkgRef where
  format HkgRef {..} = format name <> "-" <> format version
