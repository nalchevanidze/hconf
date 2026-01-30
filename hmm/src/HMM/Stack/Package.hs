{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Stack.Package
  ( Package (..),
    syncPackages,
    resolvePackages,
    publishPackages,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import HMM.Core.Bounds (BoundsByName)
import HMM.Core.Dependencies (Dependencies)
import HMM.Core.PkgDir (PkgDir, packageFile)
import HMM.Core.Version (Version, readVersion)
import HMM.Stack.Cabal (Cabal (..), CabalSrc (..), upload)
import HMM.Stack.Lib (Libraries, Library, updateDependencies, updateLibrary)
import HMM.Utils.Class (Check (..), format)
import HMM.Utils.Core (PkgName, aesonYAMLOptions, throwError, tupled)
import HMM.Utils.FromConf (ReadConf, readList)
import HMM.Utils.Log (task)
import HMM.Utils.Yaml (readYaml, rewrite)
import Relude hiding (Undefined, length, replicate)

data Package = Package
  { name :: PkgName,
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
  task (toString pkgDir) $ do
    Package {..} <- rewritePackage pkgDir
    check CabalSrc {pkgDir, target = Cabal {..}}

publishPackage :: (ReadConf m '[Version, BoundsByName]) => PkgDir -> m ()
publishPackage path = do
  Package {name} <- readYaml $ packageFile path
  task (toString name) $ upload name []

syncPackages :: (ReadConf m '[Version, BoundsByName]) => m ()
syncPackages = task "packages" $ readList >>= traverse_ checkPackage

publishPackages :: (ReadConf m '[Version, BoundsByName]) => m ()
publishPackages = task "packages" $ readList >>= traverse_ publishPackage