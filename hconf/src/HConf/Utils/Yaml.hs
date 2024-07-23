{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Yaml
  ( readYaml,
    rewrite,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
  )
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import HConf.Utils.Class (HConfIO (..))
import HConf.Utils.Core (compareFields, withThrow)
import HConf.Utils.Log (logFileChange)
import Relude hiding (Show, Undefined, intercalate, show)
import Prelude (Show (..))

serializeYaml :: (ToJSON a) => a -> ByteString
serializeYaml =
  encodePretty
    $ setConfDropNull True
    $ setConfCompare compareFields defConfig

data Yaml t = Yaml
  { getData :: t,
    rawValue :: Object
  }
  deriving (Generic)

instance (Show t) => Show (Yaml t) where
  show (Yaml t _) = show t

instance (FromJSON t) => FromJSON (Yaml t) where
  parseJSON v = Yaml <$> parseJSON v <*> parseJSON v

instance (ToJSON t) => ToJSON (Yaml t) where
  toJSON (Yaml t v) = Object (toObject (toJSON t) <> v)

toObject :: Value -> Object
toObject (Object x) = x
toObject _ = mempty

mapYaml :: (Functor m) => (Maybe t -> m t) -> Maybe (Yaml t) -> m (Yaml t)
mapYaml f (Just (Yaml v props)) = (`Yaml` props) <$> f (Just v)
mapYaml f Nothing = (`Yaml` mempty) <$> f Nothing

fromEither :: (HConfIO m, FromJSON b) => Either a ByteString -> m (Maybe b)
fromEither = either (const $ pure Nothing) (fmap Just . liftIO . decodeThrow)

rewrite :: (HConfIO m, FromJSON t, ToJSON t) => FilePath -> (Maybe t -> m t) -> m t
rewrite pkg f = do
  original <- read pkg
  yaml <- fromEither original >>= mapYaml f
  let newFile = serializeYaml yaml
  logFileChange pkg (fromRight "" original == newFile)
  withThrow (write pkg newFile)
  pure (getData yaml)

readYaml :: (FromJSON a, HConfIO m) => FilePath -> m a
readYaml = withThrow . read >=> (liftIO . decodeThrow)
