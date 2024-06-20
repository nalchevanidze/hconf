{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Class
  ( Parse (..),
    Check (..),
    HConfIO (..),
    FromConf (..),
    readPackages,
  )
where

import Control.Exception (tryJust)
import qualified Data.ByteString as L
import HConf.Utils.Core (Name, PkgDir (..), maybeToError)
import Relude

class Parse a where
  parse :: (MonadFail m) => Text -> m a

instance Parse Int where
  parse t =
    maybeToError
      ("could not parse Int: " <> t <> "'!")
      (readMaybe $ toString t)

readPackages :: (FromConf m [PkgDir]) => m [PkgDir]
readPackages = fromConf

class (MonadFail m, HConfIO m) => FromConf m a where
  fromConf :: m a

class Check a where
  check :: (MonadFail m, MonadIO m, FromConf m [PkgDir]) => a -> m ()

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
