{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Stack.Lib
  ( Library (..),
    updateDependencies,
    updateLibrary,
    Libraries,
  )
where

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (delete)
# else
import Data.HashMap.Lazy (delete)
#endif
import Data.Aeson.Types
  ( FromJSON (..),
    GFromJSON,
    Object,
    Parser,
    ToJSON (..),
    Value (..),
    Zero,
    genericParseJSON,
    genericToJSON,
    withObject,
  )
import GHC.Generics (Generic (..))
import HMM.Core.Bounds (Bounds, BoundsByName)
import HMM.Core.Dependencies (Dependencies, traverseDeps)
import HMM.Utils.Class (logDiff)
import HMM.Utils.Core (DependencyName, Name, aesonYAMLOptions)
import HMM.Utils.FromConf (ReadConf, readByKey)
import HMM.Utils.Log (field)
import Relude

type Libraries = Map Name Library

data Library = Library
  { sourceDirs :: Name,
    dependencies :: Maybe Dependencies,
    __unknownFields :: Maybe Object
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON Library where
  parseJSON = fromObject (\t o -> t {__unknownFields = o})

instance ToJSON Library where
  toJSON t = Object (toObject (genericToJSON aesonYAMLOptions t) <> fromMaybe mempty (__unknownFields t))

fromObject :: (Generic a, GFromJSON Zero (Rep a)) => (a -> Maybe Object -> a) -> Value -> Parser a
fromObject f v = do
  t <- genericParseJSON aesonYAMLOptions v
  o <- withObject "Lib" pure v
  pure (f t (Just o))

toObject :: Value -> Object
toObject (Object x) = delete "__unknown-fields" x
toObject _ = mempty

updateDependency :: (ReadConf m BoundsByName) => DependencyName -> Bounds -> m Bounds
updateDependency name oldBounds = do
  bounds <- readByKey name
  logDiff (field name) oldBounds bounds
  pure bounds

updateDependencies :: (ReadConf m BoundsByName) => Dependencies -> m Dependencies
updateDependencies = traverseDeps updateDependency

updateLibrary :: (ReadConf m BoundsByName) => Library -> m Library
updateLibrary Library {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ Library {dependencies = newDependencies, ..}
