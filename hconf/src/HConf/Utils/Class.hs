{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Class
  ( Parse (..),
    Check (..),
    HConfIO (..),
    ReadConf (..),
  )
where

import Control.Exception (tryJust)
import qualified Data.ByteString as L
import HConf.Utils.Core (Name)
import Relude

class Parse a where
  parse :: (MonadFail m) => String -> m a
  parseText :: (MonadFail m) => Text -> m a

class (Monad m, MonadFail m, HConfIO m) => ReadConf m where
  packages :: m [Name]

class Check a where
  check :: (MonadFail m, ReadConf m, MonadIO m) => a -> m ()

class (MonadIO m, MonadFail m) => HConfIO m where
  eitherRead :: FilePath -> m (Either String ByteString)
  read :: FilePath -> m ByteString
  write :: FilePath -> ByteString -> m ()

printException :: SomeException -> String
printException = show

instance HConfIO IO where
  eitherRead path = tryJust (Just . printException) (L.readFile path)
  read = L.readFile
  write = L.writeFile
