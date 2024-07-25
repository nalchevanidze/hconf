{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Class
  ( Parse (..),
    Check (..),
    HConfIO (..),
    ReadFromConf (..),
    Log (..),
    Format (..),
    Diff (..),
    logDiff,
    ReadConf,
    readList,
    readEnv,
    readByKey,
    ByKey (..),
  )
where

import Control.Exception (catch, throwIO)
import Data.ByteString (readFile, writeFile)
import HConf.Core.Env (Env)
import HConf.Core.PkgDir (PkgDir)
import HConf.Utils.Core (maybeToError, safeIO)
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

readList :: (ReadConf m [a]) => m [a]
readList = readFromConf ()

readEnv :: (ReadConf m Env) => (Env -> a) -> m a
readEnv f = f <$> readFromConf ()

readByKey :: (ReadConf m (ByKey k a)) => k -> m a
readByKey = unpackKey readFromConf

unpackKey :: (Functor m) => (k -> m (ByKey k a)) -> k -> m a
unpackKey f k = byKey <$> f k

newtype ByKey k a = ByKey {byKey :: a}

type family Key a :: Type where
  Key (ByKey k a) = k
  Key a = ()

class (HConfIO m) => ReadFromConf m a where
  readFromConf :: Key a -> m a

class ReadConfFuncDef m a where
  type ReadConf m a :: Constraint

instance ReadConfFuncDef (m :: Type -> Type) (a :: Type) where
  type ReadConf m a = ReadConfFunc m '[a]

instance ReadConfFuncDef (m :: Type -> Type) (a :: [Type]) where
  type ReadConf m a = ReadConfFunc m a

type family ReadConfFunc m a where
  ReadConfFunc m '[()] = ReadConfFunc m '[]
  ReadConfFunc m '[] = ReadFromConf m [PkgDir]
  ReadConfFunc m (x : xs) = (ReadFromConf m x, ReadConfFunc m xs)

class Check m a where
  check :: a -> m ()

class (MonadIO m, MonadFail m) => HConfIO m where
  read :: FilePath -> m (Either String ByteString)
  write :: FilePath -> ByteString -> m (Either String ())
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

class Diff a where
  diff :: a -> a -> Maybe String

logDiff :: (Diff a, Monad m) => (String -> m ()) -> a -> a -> m ()
logDiff f a = maybe (pure ()) f . diff a
