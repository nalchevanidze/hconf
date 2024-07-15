{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.ConfigT
  ( ConfigT (..),
    HCEnv (..),
    save,
    run,
    runTask,
  )
where

import Control.Exception (tryJust)
import HConf.Config.Build (Builds)
import HConf.Config.Config (Config (..), getRule)
import HConf.Config.PkgGroup (pkgDirs)
import HConf.Core.Bounds (ReadBounds (..))
import HConf.Core.Env (Env (..))
import HConf.Core.PkgDir (PkgDir)
import HConf.Core.Version (Version)
import HConf.Utils.Chalk (Color (Green), chalk)
import HConf.Utils.Class
  ( Check (..),
    FromConf (..),
    HConfIO (..),
  )
import HConf.Utils.Core (Name)
import HConf.Utils.Log
  ( Log (..),
    alert,
    task,
  )
import HConf.Utils.Yaml (readYaml, rewrite)
import Relude

data HCEnv = HCEnv
  { config :: Config,
    env :: Env,
    indention :: Int
  }

newtype ConfigT (a :: Type) = ConfigT {_runConfigT :: ReaderT HCEnv IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader HCEnv,
      MonadIO,
      MonadFail
    )

printException :: SomeException -> String
printException = show

runConfigT :: ConfigT a -> Env -> Config -> IO (Either String a)
runConfigT (ConfigT (ReaderT f)) env config = tryJust (Just . printException) (f HCEnv {indention = 0, ..})

indent :: Int -> String
indent i = replicate (i * 2) ' '

instance Log ConfigT where
  log txt = do
    i <- asks indention
    liftIO $ putStrLn $ indent i <> txt
  inside f m = do
    i <- asks indention
    log (f i)
    local (\c -> c {indention = indention c + 1}) m

instance HConfIO ConfigT where
  read = liftIO . read
  write f = liftIO . write f
  remove = liftIO . remove

instance ReadBounds ConfigT where
  readBounds name = asks config >>= getRule name

run :: (ToString a) => ConfigT (Maybe a) -> Env -> IO ()
run m env@Env {..} = do
  cfg <- readYaml hconf
  runConfigT (asks config >>= check >> m) env cfg >>= handle

runTask :: Name -> ConfigT () -> Env -> IO ()
runTask name m = run (task name m $> Just (chalk Green "\nOk"))

handle :: (ToString a) => (HConfIO m) => Either String (Maybe a) -> m ()
handle res = case res of
  Left x -> alert ("ERROR: " <> x)
  (Right Nothing) -> pure ()
  (Right (Just msg)) -> log (toString msg)

save :: Config -> ConfigT ()
save cfg = task "save" $ task "hconf.yaml" $ do
  ctx <- asks id
  rewrite (hconf $ env ctx) (const $ pure cfg) $> ()

instance FromConf ConfigT [PkgDir] where
  fromConf = concatMap pkgDirs <$> asks (groups . config)

instance FromConf ConfigT Builds where
  fromConf = asks (builds . config)

instance FromConf ConfigT Env where
  fromConf = asks env

instance FromConf ConfigT Version where
  fromConf = asks (version . config)
