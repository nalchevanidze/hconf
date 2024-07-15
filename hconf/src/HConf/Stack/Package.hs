{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Stack.Package
  ( Package (..),
    checkPackages,
    resolvePackages,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import HConf.Core.Bounds (Bounds)
import HConf.Core.Dependencies (Dependencies)
import HConf.Core.PkgDir (PkgDir, packageFile)
import HConf.Core.Version (Version)
import HConf.Stack.Cabal (Cabal (..), CabalSrc (..))
import HConf.Stack.Lib (Libraries, Library, updateDependencies, updateLibrary)
import HConf.Utils.Class (Check (..), FCon, confList, fromConf)
import HConf.Utils.Core (Name, aesonYAMLOptions, throwError, tupled)
import HConf.Utils.Log (task)
import HConf.Utils.Yaml (readYaml, rewrite)
import Relude hiding (Undefined, length, replicate)

data Package = Package
  { name :: Name,
    version :: Version,
    library :: Maybe Library,
    dependencies :: Dependencies,
    tests :: Maybe Libraries,
    executables :: Maybe Libraries,
    benchmarks :: Maybe Libraries
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON Package where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Package where
  toJSON = genericToJSON aesonYAMLOptions

resolvePackages :: (FCon m ()) => m [(PkgDir, Package)]
resolvePackages = confList >>= traverse (tupled (readYaml . packageFile))

updateLibraries :: (FCon m Bounds) => Maybe Libraries -> m (Maybe Libraries)
updateLibraries = traverse (traverse updateLibrary)

updatePackage :: (FCon m '[Version, Bounds]) => Maybe Package -> m Package
updatePackage Nothing = throwError "could not find package file"
updatePackage (Just Package {..}) = do
  newLibrary <- traverse updateLibrary library
  newTests <- updateLibraries tests
  newExecutables <- updateLibraries executables
  newBenchmarks <- updateLibraries benchmarks
  newDependencies <- updateDependencies dependencies
  newVersion <- fromConf
  pure
    $ Package
      { version = newVersion,
        library = newLibrary,
        tests = newTests,
        executables = newExecutables,
        benchmarks = newBenchmarks,
        dependencies = newDependencies,
        ..
      }

rewritePackage :: (FCon m '[Version, Bounds]) => PkgDir -> m Package
rewritePackage path = task "package" $ rewrite (packageFile path) updatePackage

checkPackage :: (FCon m '[Version, Bounds]) => PkgDir -> m ()
checkPackage pkgDir =
  task (toText pkgDir) $ do
    Package {..} <- rewritePackage pkgDir
    check CabalSrc {pkgDir, target = Cabal {..}}

checkPackages :: (FCon m '[Version, Bounds]) => m ()
checkPackages = task "packages" $ confList >>= traverse_ checkPackage
