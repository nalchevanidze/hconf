{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Class
  ( Parse (..),
    Check (..),
    HConfIO (..),
    Log (..),
    Format (..),
    Diff (..),
    logDiff,
  )
where

import Control.Exception (catch, throwIO)
import Data.ByteString (readFile, writeFile)
import HConf.Utils.Core (DependencyName (..), PkgName (..), Result, maybeToError, safeIO)
import Relude hiding (readFile, writeFile)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

class Log a where
  log :: (HConfIO m) => a -> m ()

instance (Log a) => Log [a] where
  log = traverse_ log

class Parse a where
  parse :: (MonadFail m) => Text -> m a

instance Parse Int where
  parse t =
    maybeToError
      ("could not parse Int: " <> t <> "'!")
      (readMaybe $ toString t)

instance Parse DependencyName where
  parse = pure . DependencyName

instance Parse PkgName where
  parse = pure . PkgName

class Check m a where
  check :: a -> m ()

class (MonadIO m, MonadFail m) => HConfIO m where
  read :: FilePath -> m (Result ByteString)
  write :: FilePath -> ByteString -> m (Result ())
  remove :: FilePath -> m ()
  putLine :: String -> m ()
  inside :: (Int -> String) -> m a -> m a

instance HConfIO IO where
  read = safeIO . readFile
  write f = safeIO . writeFile f
  remove file = removeFile file `catch` (\e -> unless (isDoesNotExistError e) (throwIO e))
  putLine = putStrLn
  inside _ = id

class Format a where
  format :: a -> Text

instance Format Int where
  format = show

instance Format DependencyName where
  format (DependencyName x) = x

instance Format PkgName where
  format (PkgName x) = x

class Diff a where
  diff :: a -> a -> Maybe String

logDiff :: (Diff a, Monad m) => (String -> m ()) -> a -> a -> m ()
logDiff f a = maybe (pure ()) f . diff a
