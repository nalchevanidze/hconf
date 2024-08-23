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
import HConf.Core.Bounds (BoundsByName)
import HConf.Core.Dependencies (Dependencies)
import HConf.Core.PkgDir (PkgDir, packageFile)
import HConf.Core.Version (Version, readVersion)
import HConf.Stack.Cabal (Cabal (..), CabalSrc (..))
import HConf.Stack.Lib (Libraries, Library, updateDependencies, updateLibrary)
import HConf.Utils.Class (Check (..))
import HConf.Utils.Core (Name, aesonYAMLOptions, throwError, tupled)
import HConf.Utils.FromConf (ReadConf, readList)
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

resolvePackages :: (ReadConf m ()) => m [(PkgDir, Package)]
resolvePackages = readList >>= traverse (tupled (readYaml . packageFile))

updateLibraries :: (ReadConf m BoundsByName) => Maybe Libraries -> m (Maybe Libraries)
updateLibraries = traverse (traverse updateLibrary)

updatePackage :: (ReadConf m '[Version, BoundsByName]) => Maybe Package -> m Package
updatePackage Nothing = throwError "could not find package file"
updatePackage (Just Package {..}) = do
  newLibrary <- traverse updateLibrary library
  newTests <- updateLibraries tests
  newExecutables <- updateLibraries executables
  newBenchmarks <- updateLibraries benchmarks
  newDependencies <- updateDependencies dependencies
  newVersion <- readVersion
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

rewritePackage :: (ReadConf m '[Version, BoundsByName]) => PkgDir -> m Package
rewritePackage path = task "package" $ rewrite (packageFile path) updatePackage

checkPackage :: (ReadConf m '[Version, BoundsByName]) => PkgDir -> m ()
checkPackage pkgDir =
  task (toText pkgDir) $ do
    Package {..} <- rewritePackage pkgDir
    check CabalSrc {pkgDir, target = Cabal {..}}

checkPackages :: (ReadConf m '[Version, BoundsByName]) => m ()
checkPackages = task "packages" $ readList >>= traverse_ checkPackage
