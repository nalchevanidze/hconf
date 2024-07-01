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
import HConf.Core.Bounds (Bounds, ReadBounds (..), diff)
import HConf.Core.Dependencies (Dependencies, traverseDeps)
import HConf.Utils.Core (Name, aesonYAMLOptions)
import HConf.Utils.Log (Log, field)
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

withRule :: (MonadIO m, Log m) => Name -> Bounds -> Bounds -> m Bounds
withRule name oldBounds bounds = printDiff (field name) (diff oldBounds bounds) $> bounds

printDiff :: Monad m => (a -> m ()) -> Maybe a -> m ()
printDiff = maybe (pure ())

updateDependency :: (ReadBounds m) => Name -> Bounds -> m Bounds
updateDependency name oldBounds =
  readBounds name
    >>= withRule name oldBounds

updateDependencies :: (ReadBounds m) => Dependencies -> m Dependencies
updateDependencies = traverseDeps updateDependency

updateLibrary :: (ReadBounds m) => Library -> m Library
updateLibrary Library {..} = do
  newDependencies <- traverse updateDependencies dependencies
  pure $ Library {dependencies = newDependencies, ..}
