{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Stack.Lib
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
import HConf.Core.Bounds (Bounds, BoundsByName)
import HConf.Core.Dependencies (Dependencies, traverseDeps)
import HConf.Utils.Class (ReadConf, logDiff, readByName)
import HConf.Utils.Core (Name, aesonYAMLOptions)
import HConf.Utils.Log (field)
import Relude hiding
  ( Undefined,
    break,
    drop,
    intercalate,
    isPrefixOf,
    length,
    null,
  )

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

updateDependency :: (ReadConf m BoundsByName) => Name -> Bounds -> m Bounds
updateDependency name oldBounds = do
  bounds <- readByName name
  logDiff (field name) oldBounds bounds
  pure bounds

updateDependencies :: (ReadConf m BoundsByName) => Dependencies -> m Dependencies
updateDependencies = traverseDeps updateDependency

updateLibrary :: (ReadConf m BoundsByName) => Library -> m Library
updateLibrary Library {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ Library {dependencies = newDependencies, ..}
