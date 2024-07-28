{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.HkgRef
  ( fetchVersions,
    hkgRefs,
    HkgRef,
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
  ( Name,
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

data HkgRef = HkgRef
  { name :: Name,
    version :: Version
  }

fetchVersions :: (HConfIO m) => Name -> m Versions
fetchVersions name = hackage ["package", name, "preferred"] >>= getField "normal-version"

instance (HConfIO m) => Check m HkgRef where
  check HkgRef {..} = fetchVersions name >>= checkElem "version" name version . toList

hkgRefs :: Map Name Version -> [HkgRef]
hkgRefs = map (uncurry HkgRef) . M.toList

instance Format HkgRef where
  format HkgRef {..} = name <> "-" <> format version
