{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Config.ConfigT
  ( ConfigT (..),
    HCEnv (..),
    save,
    run,
    runTask,
  )
where

import Control.Exception (tryJust)
import HMM.Config.Build (Builds)
import HMM.Config.Config (Config (..), getRule)
import HMM.Config.PkgGroup (pkgDirs)
import HMM.Core.Bounds (Bounds)
import HMM.Core.Env (Env (..))
import HMM.Core.PkgDir (PkgDirs)
import HMM.Core.Version (Version)
import HMM.Utils.Chalk (Color (Green), chalk)
import HMM.Utils.Class
  ( Check (..),
    HIO (..),
  )
import HMM.Utils.Core (DependencyName (..), printException)
import HMM.Utils.FromConf (ByKey (..), ReadFromConf (..))
import HMM.Utils.Log
  ( alert,
    task,
  )
import HMM.Utils.Yaml (readYaml, rewrite)
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

runConfigT :: ConfigT a -> Env -> Config -> IO (Either String a)
runConfigT (ConfigT (ReaderT f)) env config = tryJust (Just . printException) (f HCEnv {indention = 0, ..})

indent :: Int -> String -> String
indent i = (replicate (i * 2) ' ' <>)

instance HIO ConfigT where
  read = liftIO . read
  write f = liftIO . write f
  remove = liftIO . remove
  putLine txt = asks indention >>= liftIO . putLine . flip indent txt
  inside f m = do
    asks indention >>= putLine . f
    local (\c -> c {indention = indention c + 1}) m

run :: (ToString a) => ConfigT (Maybe a) -> Env -> IO ()
run m env@Env {..} = do
  cfg <- readYaml hconf
  runConfigT (asks config >>= check >> m) env cfg >>= handle

runTask :: String -> ConfigT () -> Env -> IO ()
runTask name m = run (task name m $> Just (chalk Green "\nOk"))

handle :: (ToString a) => (HIO m) => Either String (Maybe a) -> m ()
handle res = case res of
  Left x -> do
    alert ("ERROR: " <> x)
    liftIO exitFailure
  (Right Nothing) -> pure ()
  (Right (Just msg)) -> putLine (toString msg)

save :: Config -> ConfigT ()
save cfg = task "save" $ task "hconf.yaml" $ do
  ctx <- asks id
  rewrite (hconf $ env ctx) (const $ pure cfg) $> ()

instance ReadFromConf ConfigT PkgDirs where
  readFromConf = const $ concatMap pkgDirs <$> asks (groups . config)

instance ReadFromConf ConfigT Builds where
  readFromConf = const $ asks (builds . config)

instance ReadFromConf ConfigT Env where
  readFromConf = const $ asks env

instance ReadFromConf ConfigT Version where
  readFromConf = const $ asks (version . config)

instance ReadFromConf ConfigT (ByKey DependencyName Bounds) where
  readFromConf name = ByKey <$> (asks config >>= getRule name)
