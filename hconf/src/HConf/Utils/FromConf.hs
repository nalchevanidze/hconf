{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.FromConf
  ( ReadFromConf (..),
    ReadConf,
    readList,
    readEnv,
    readByKey,
    ByKey (..),
  )
where

import HConf.Core.Env (Env)
import HConf.Core.PkgDir (PkgDirs)
import HConf.Utils.Class (HConfIO)
import Relude

readList :: (ReadConf m [a]) => m [a]
readList = readFromConf ()

readEnv :: (ReadConf m Env) => (Env -> a) -> m a
readEnv f = f <$> readFromConf ()

readByKey :: (ReadConf m (ByKey k a)) => k -> m a
readByKey = unpackKey readFromConf

unpackKey :: (Functor m) => (k -> m (ByKey k a)) -> k -> m a
unpackKey f k = byKey <$> f k

class ReadConfFuncDef m a where
  type ReadConf m a :: Constraint

instance ReadConfFuncDef (m :: Type -> Type) (a :: Type) where
  type ReadConf m a = ReadConfFunc m '[a]

instance ReadConfFuncDef (m :: Type -> Type) (a :: [Type]) where
  type ReadConf m a = ReadConfFunc m a

type family ReadConfFunc m a where
  ReadConfFunc m '[()] = ReadConfFunc m '[]
  ReadConfFunc m '[] = ReadFromConf m PkgDirs
  ReadConfFunc m (x : xs) = (ReadFromConf m x, ReadConfFunc m xs)

newtype ByKey k a = ByKey {byKey :: a}

type family Key a :: Type where
  Key (ByKey k a) = k
  Key a = ()

class (HConfIO m) => ReadFromConf m a where
  readFromConf :: Key a -> m a
