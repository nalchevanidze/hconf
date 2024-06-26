{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
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
import HConf.Utils.Class (BaseM, Check (..), FromConf (..), readPackages)
import HConf.Utils.Core (Name, aesonYAMLOptions, tupled)
import HConf.Utils.Log (Log, label, task)
import HConf.Utils.Yaml (readYaml, rewriteYaml)
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

resolvePackages :: (FromConf m [PkgDir], Log m) => m [(PkgDir, Package)]
resolvePackages = fromConf >>= traverse (tupled (readYaml . packageFile))

updateLibraries :: (ReadBounds m) => Maybe Libraries -> m (Maybe Libraries)
updateLibraries = traverse (traverse updateLibrary)

updatePackage :: (ReadBounds m, FromConf m Version) => Package -> m Package
updatePackage Package {..} = do
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

rewritePackage :: (ReadBounds m, FromConf m Version) => PkgDir -> m Package
rewritePackage path =
  task "package"
    $ rewriteYaml (packageFile path) updatePackage

checkPackage :: (ReadBounds m, BaseM m, FromConf m Version) => PkgDir -> m ()
checkPackage pkgDir =
  task (toText pkgDir) $ do
    Package {..} <- rewritePackage pkgDir
    check CabalSrc {pkgDir, target = Cabal {..}}

checkPackages :: (ReadBounds m, FromConf m Version, BaseM m) => m ()
checkPackages =
  label "packages"
    $ readPackages
    >>= traverse_ checkPackage
