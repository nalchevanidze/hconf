{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Yaml
  ( readYaml,
    writeYaml,
    rewriteYaml,
    removeIfExists,
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

mapYaml :: (Functor m) => (t -> m t) -> Yaml t -> m (Yaml t)
mapYaml f (Yaml v props) = (`Yaml` props) <$> f v

rewriteYaml :: (HConfIO m, Log m, FromJSON t, ToJSON t) => FilePath -> (t -> m t) -> m t
rewriteYaml pkg f = readYaml pkg >>= mapYaml f >>= \x -> writeYaml pkg x >> pure (getData x)

removeIfExists :: FilePath -> IO ()
removeIfExists name = removeFile name `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e
