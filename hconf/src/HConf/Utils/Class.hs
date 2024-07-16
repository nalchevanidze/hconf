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
    readList,
    readEnv,
    readByName,
    ByName(..),
  )
where

import Control.Exception (catch, throwIO, tryJust)
import Data.ByteString (readFile, writeFile)
import HConf.Core.Env (Env)
import HConf.Core.PkgDir (PkgDir)
import HConf.Utils.Core (Msg (..), Name, maybeToError, throwError)
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

readList :: (ReadConf m [a]) => m [a]
readList = lookupConf ()

readEnv :: (ReadConf m Env) => (Env -> a) -> m a
readEnv f = f <$> lookupConf ()

readByName :: (ReadConf m (ByName a)) => Name -> m a
readByName name = byId <$> lookupConf name

newtype ByName a = ByName {byId :: a}

type family LookupKey a :: Type where
  LookupKey (ByName a) = Name
  LookupKey a = ()

class (HConfIO m) => LookupConf m a where
  lookupConf :: LookupKey a -> m a

class ReadConfFuncDef m a where
  type ReadConf m a :: Constraint

instance ReadConfFuncDef (m :: Type -> Type) (a :: Type) where
  type ReadConf m a = ReadConfFunc m '[a]

instance ReadConfFuncDef (m :: Type -> Type) (a :: [Type]) where
  type ReadConf m a = ReadConfFunc m a

type family ReadConfFunc m a where
  ReadConfFunc m '[()] = ReadConfFunc m '[]
  ReadConfFunc m '[] = LookupConf m [PkgDir]
  ReadConfFunc m (x : xs) = (LookupConf m x, ReadConfFunc m xs)

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
