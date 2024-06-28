{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Hie
  ( genHie,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object)
import qualified Data.Map as M
import HConf.Core.Env (Env (..))
import HConf.Core.PkgDir (PkgDir, pkgFile)
import HConf.Stack.Lib (Libraries, Library (..))
import HConf.Stack.Package (Package (..), resolvePackages)
import HConf.Utils.Class (FromConf (fromConf))
import HConf.Utils.Log (Log, task)
import HConf.Utils.Yaml (rewrite)
import Relude hiding (Undefined, intercalate)

data Component = Component
  { path :: FilePath,
    component :: Text
  }
  deriving
    ( ToJSON,
      FromJSON,
      Generic,
      Show
    )

data Components = Components
  { stackYaml :: FilePath,
    components :: [Component]
  }
  deriving
    ( ToJSON,
      FromJSON,
      Generic,
      Show
    )

packHie :: Components -> Value
packHie value = object [("cradle", object [("stack", toJSON value)])]

(<:>) :: (Semigroup a, IsString a) => a -> a -> a
(<:>) name tag = name <> ":" <> tag

toLib :: (PkgDir, Package) -> [Component]
toLib (path, Package {..}) =
  comp "lib" library
    <> compGroup "test" tests
    <> compGroup "exe" executables
    <> compGroup "bench" benchmarks
  where
    compGroup :: Text -> Maybe Libraries -> [Component]
    compGroup tag = concatMap mkComp . concatMap M.toList . maybeToList
      where
        mkComp (k, lib) = comp (tag <:> k) (Just lib)
    comp :: Text -> Maybe Library -> [Component]
    comp tag (Just Library {sourceDirs}) =
      [ Component
          { path = "./" <> pkgFile (toString sourceDirs) path,
            component = name <:> tag
          }
      ]
    comp _ _ = []

genHie :: (FromConf m Env, Log m, FromConf m [PkgDir]) => m ()
genHie = task "hie"
  $ task "hie.yaml"
  $ do
    Env {..} <- fromConf
    components <- concatMap toLib <$> resolvePackages
    rewrite hie (const $ pure $ packHie Components {stackYaml = stack, components}) $> ()
