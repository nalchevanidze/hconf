{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( setup,
    Env (..),
    updateVersion,
    VersionTag (..),
    Parse (..),
    getVersion,
    upperBounds,
    format,
  )
where

import HConf.Config.Config (Config (..), updateConfig, updateConfigUpperBounds)
import HConf.Config.ConfigT (HCEnv (..), run, runTask, save)
import HConf.Config.Tag (VersionTag (..))
import HConf.Core.Env (Env (..))
import HConf.Format (formatWith)
import HConf.Hie (genHie)
import HConf.Stack.Config (setupStack)
import HConf.Stack.Package (checkPackages)
import HConf.Utils.Class (Parse (..))
import Relude hiding (fix)

format :: Bool -> Env -> IO ()
format fix = runTask "format" $ formatWith fix

upperBounds :: Env -> IO ()
upperBounds =
  runTask "upper-bounds"
    $ asks config
    >>= updateConfigUpperBounds
    >>= save

setup :: VersionTag -> Env -> IO ()
setup v = runTask "setup" $ do
  setupStack v
  genHie
  checkPackages

updateVersion :: Bool -> Env -> IO ()
updateVersion isBreaking = runTask "next" $ (asks config <&> updateConfig isBreaking) >>= save

getVersion :: Env -> IO ()
getVersion = run (Just . version <$> asks config)
