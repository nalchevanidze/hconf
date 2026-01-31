{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Core.HkgRef
  ( lookupVersions,
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
import HMM.Utils.FromConf
import HMM.Utils.Http (hackage)
import Relude hiding
  ( Undefined,
    break,
    drop,
    fromList,
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

lookupVersions :: (HIO m, ReadConf m '[VersionsMap]) => DependencyName -> m Versions
lookupVersions name = do
  versionsMap <- readFromConf ()
  case M.lookup name versionsMap of
    Just versions -> pure versions
    Nothing -> hackage ["package", format name, "preferred"] >>= getField "normal-version"

instance (HIO m, ReadConf m '[VersionsMap]) => Check m HkgRef where
  check HkgRef {..} = lookupVersions name >>= checkElem "version" (format name) version . toList

hkgRefs :: VersionMap -> [HkgRef]
hkgRefs = map (uncurry HkgRef) . M.toList

instance Format HkgRef where
  format HkgRef {..} = format name <> "-" <> format version
