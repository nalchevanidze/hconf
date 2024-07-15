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
import HConf.Core.Bounds (ReadBounds (..))
import HConf.Core.Dependencies (Dependencies)
import HConf.Core.PkgDir (PkgDir, packageFile)
import HConf.Core.Version (Version)
import HConf.Stack.Cabal (Cabal (..), CabalSrc (..))
import HConf.Stack.Lib (Libraries, Library, updateDependencies, updateLibrary)
import HConf.Utils.Class (Check (..), FCon, fromConf, packages)
import HConf.Utils.Core (Name, aesonYAMLOptions, throwError, tupled)
import HConf.Utils.Log (Log, task)
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

resolvePackages :: (FCon m (), Log m) => m [(PkgDir, Package)]
resolvePackages = fromConf >>= traverse (tupled (readYaml . packageFile))

updateLibraries :: (ReadBounds m) => Maybe Libraries -> m (Maybe Libraries)
updateLibraries = traverse (traverse updateLibrary)

updatePackage :: (ReadBounds m, FCon m Version) => Maybe Package -> m Package
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

rewritePackage :: (ReadBounds m, FCon m Version) => PkgDir -> m Package
rewritePackage path = task "package" $ rewrite (packageFile path) updatePackage

checkPackage :: (ReadBounds m, Log m, FCon m Version) => PkgDir -> m ()
checkPackage pkgDir =
  task (toText pkgDir) $ do
    Package {..} <- rewritePackage pkgDir
    check CabalSrc {pkgDir, target = Cabal {..}}

checkPackages :: (ReadBounds m, FCon m Version, Log m) => m ()
checkPackages = task "packages" $ packages >>= traverse_ checkPackage
