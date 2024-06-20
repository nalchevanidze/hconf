{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
import HConf.Core.Version (Version)
import HConf.Stack.Cabal (checkCabal)
import HConf.Stack.Lib (Libraries, Library, updateDependencies, updateLibrary)
import HConf.Utils.Class (FromConf (..), readPackages)
import HConf.Utils.Core (Name, PkgName (..), aesonYAMLOptions, tupled)
import HConf.Utils.Log (Log, label, subTask, task)
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

toPath :: PkgName -> FilePath
toPath (PkgName name) = toString name <> "/package.yaml"

resolvePackages :: (FromConf m [PkgName], Log m) => m [(PkgName, Package)]
resolvePackages = fromConf >>= traverse (tupled (readYaml . toPath))

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

rewritePackage :: (ReadBounds m, FromConf m Version) => PkgName -> m Package
rewritePackage path =
  subTask "package"
    $ rewriteYaml (toPath path) updatePackage

checkPackage :: (ReadBounds m, FromConf m Version) => Name -> m ()
checkPackage path =
  task (toText path) $ do
    Package {..} <- rewritePackage (PkgName path)
    checkCabal (toText path) name version

checkPackages :: (ReadBounds m, FromConf m Version, FromConf m [PkgName]) => m ()
checkPackages =
  label "packages"
    $ readPackages
    >>= traverse_ checkPackage
