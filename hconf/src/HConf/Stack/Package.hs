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
import Data.Text (unpack)
import HConf.Core.Bounds (ReadBounds (..))
import HConf.Core.Dependencies (Dependencies)
import HConf.Core.Version (Version)
import HConf.Stack.Cabal (checkCabal)
import HConf.Stack.Lib (Libraries, Library, updateDependencies, updateLibrary)
import HConf.Utils.Class (FromConf (..), ReadConf (..))
import HConf.Utils.Core (Name, aesonYAMLOptions, tupled)
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

toPath :: Name -> FilePath
toPath = (<> "/package.yaml") . unpack

resolvePackages :: (ReadConf m, Log m) => m [(Name, Package)]
resolvePackages = readPackages >>= traverse (tupled (readYaml . toPath))

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

rewritePackage :: (ReadBounds m, FromConf m Version) => Name -> m Package
rewritePackage path =
  subTask "package"
    $ rewriteYaml (toPath path) updatePackage

checkPackage :: (ReadBounds m, FromConf m Version) => Name -> m ()
checkPackage path =
  task path $ do
    Package {..} <- rewritePackage path
    checkCabal path name version

checkPackages :: (ReadBounds m, FromConf m Version) => m ()
checkPackages =
  label "packages"
    $ readPackages
    >>= traverse_ checkPackage
