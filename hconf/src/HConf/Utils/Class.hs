{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Class
  ( Parse (..),
    Check (..),
    HConfIO (..),
    FromConf (..),
    readPackages,
    ResultT,
    withThrow,
  )
where

import Control.Exception (tryJust)
import qualified Data.ByteString as L
import Data.Text (unpack)
import HConf.Core.PkgDir (PkgDir)
import HConf.Utils.Core (maybeToError)
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
  read :: FilePath -> m (Either String ByteString)
  write :: FilePath -> ByteString -> m (Either String ())

printException :: SomeException -> String
printException = show

safeIO :: IO a -> IO (Either String a)
safeIO = tryJust (Just . printException)

type ResultT = ExceptT String

withThrow :: (HConfIO m) => m (Either String a) -> m a
withThrow x = x >>= either fail pure

instance HConfIO IO where
  read = safeIO . L.readFile
  write f = safeIO . L.writeFile f
