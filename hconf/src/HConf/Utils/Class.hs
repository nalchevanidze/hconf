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
    LookupConf (..),
    ResultT,
    Log (..),
    FLog (..),
    withThrow,
    Format (..),
    Diff (..),
    logDiff,
    ReadConf,
    LookupKey,
    readList,
    readEnv,
  )
where

import Control.Exception (catch, throwIO, tryJust)
import Data.ByteString (readFile, writeFile)
import HConf.Core.Env (Env)
import HConf.Core.PkgDir (PkgDir)
import HConf.Utils.Core (Msg (..), maybeToError, throwError)
import Relude hiding (readFile, writeFile)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

class FLog a where
  flog :: (Log m, Monad m) => a -> m ()

instance (FLog a) => FLog [a] where
  flog = traverse_ flog

class Log m where
  log :: String -> m ()
  inside :: (Int -> String) -> m a -> m a

instance Log IO where
  log = putStrLn
  inside _ = id

class Parse a where
  parse :: (MonadFail m) => Text -> m a

instance Parse Int where
  parse t =
    maybeToError
      ("could not parse Int: " <> t <> "'!")
      (readMaybe $ toString t)

readList :: (ReadConf m [a], LookupKey [a] ~ ()) => m [a]
readList = lookupConf ()

readEnv :: (ReadConf m Env) => (Env -> a) -> m a
readEnv f = f <$> lookupConf ()

type family LookupKey a :: Type

type instance LookupKey [k] = ()

type instance LookupKey Env = ()

class (HConfIO m) => LookupConf m a where
  lookupConf :: LookupKey a -> m a

class FC m a where
  type ReadConf m a :: Constraint

instance FC (m :: Type -> Type) (a :: Type) where
  type ReadConf m a = ReadConf' m '[a]

instance FC (m :: Type -> Type) (a :: [Type]) where
  type ReadConf m a = ReadConf' m a

type family ReadConf' m a where
  ReadConf' m '[()] = ReadConf' m '[]
  ReadConf' m '[] = LookupConf m [PkgDir]
  ReadConf' m (x : xs) = (LookupConf m x, ReadConf' m xs)

class Check m a where
  check :: a -> m ()

class (MonadIO m, MonadFail m, Log m) => HConfIO m where
  read :: FilePath -> m (Either String ByteString)
  write :: FilePath -> ByteString -> m (Either String ())
  remove :: FilePath -> m ()

printException :: SomeException -> String
printException = show

safeIO :: IO a -> IO (Either String a)
safeIO = tryJust (Just . printException)

type ResultT = ExceptT String

withThrow :: (HConfIO m) => m (Either String a) -> m a
withThrow x = x >>= either (throwError . msg) pure

instance HConfIO IO where
  read = safeIO . readFile
  write f = safeIO . writeFile f
  remove file = removeFile file `catch` (\e -> unless (isDoesNotExistError e) (throwIO e))

class Format a where
  format :: a -> Text

class Diff a where
  diff :: a -> a -> Maybe String

logDiff :: (Diff a, Monad m) => (String -> m ()) -> a -> a -> m ()
logDiff f a = maybe (pure ()) f . diff a
