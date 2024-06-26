{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Yaml
  ( readYaml,
    writeYaml,
    rewrite,
    remove,
  )
where

import Control.Exception (catch, throwIO)
import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
  )
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import HConf.Utils.Class (HConfIO (..), withThrow)
import HConf.Utils.Core (compareFields)
import HConf.Utils.Log (Log, logFileChange)
import Relude hiding (Show, Undefined, intercalate, show)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import Prelude (Show (..))

serializeYaml :: (ToJSON a) => a -> ByteString
serializeYaml =
  encodePretty
    $ setConfDropNull True
    $ setConfCompare compareFields defConfig

readYaml :: (FromJSON a, HConfIO m) => FilePath -> m a
readYaml = withThrow . read >=> (liftIO . decodeThrow)

writeYaml :: (ToJSON a, HConfIO m, Log m) => FilePath -> a -> m ()
writeYaml path v = checkAndWrite path (serializeYaml v) >>= logFileChange path

checkAndWrite :: (HConfIO m) => FilePath -> ByteString -> m Bool
checkAndWrite path newFile = do
  file <- read path
  withThrow (write path newFile)
  return (fromRight "" file == newFile)

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

from :: (MonadIO f, FromJSON a1) => Either a2 ByteString -> f (Maybe a1)
from (Left _) = pure Nothing
from (Right v) = Just <$> liftIO (decodeThrow v)

readOpt :: (HConfIO m, FromJSON a) => FilePath -> m (Maybe a)
readOpt = read >=> from

rewrite :: (HConfIO m, Log m, FromJSON t, ToJSON t) => FilePath -> (Maybe t -> m t) -> m t
rewrite pkg f = readOpt pkg >>= mapYaml f >>= \x -> writeYaml pkg x >> pure (getData x)

remove :: (MonadIO m) => FilePath -> m ()
remove name = liftIO $ removeFile name `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e
