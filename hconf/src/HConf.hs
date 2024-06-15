{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( Env (..),
    Tag (..),
    Parse (..),
    exec,
    Command (..),
    currentVersion,
    defaultConfig,
  )
where

import Data.Version (showVersion)
import HConf.Config.Config (Config (..), updateConfig, updateConfigUpperBounds)
import HConf.Config.ConfigT (HCEnv (..), run, runTask, save)
import HConf.Config.Tag (Tag (Latest))
import HConf.Core.Env (Env (..))
import HConf.Format (formatWith)
import HConf.Hie (genHie)
import HConf.Stack.Config (setupStack)
import HConf.Stack.Package (checkPackages)
import HConf.Utils.Class (Parse (..))
import qualified Paths_hconf as CLI
import Relude hiding (fix)

format :: Bool -> Env -> IO ()
format check = runTask "format" $ formatWith check

upperBounds :: Env -> IO ()
upperBounds =
  runTask "upper-bounds"
    $ asks config
    >>= updateConfigUpperBounds
    >>= save

setup :: Maybe Tag -> Env -> IO ()
setup v = runTask "setup" $ do
  setupStack (fromMaybe Latest v)
  genHie
  checkPackages

updateVersion :: Bool -> Env -> IO ()
updateVersion isBreaking = runTask "next" $ (asks config <&> updateConfig isBreaking) >>= save

getVersion :: Env -> IO ()
getVersion = run (Just . version <$> asks config)

data Command
  = Setup {tag :: Maybe Tag}
  | Next {isBreaking :: Bool}
  | UpperBounds
  | About
  | Version
  | Format {check :: Bool}
  deriving (Show)

currentVersion :: String
currentVersion = showVersion CLI.version

exec :: Env -> Command -> IO ()
exec _ About = putStrLn $ "Stack Config CLI, version " <> currentVersion
exec e Setup {tag} = setup tag e
exec e Next {isBreaking} = updateVersion isBreaking e
exec e UpperBounds = upperBounds e
exec e Version = getVersion e
exec e Format {check} = format check e

defaultConfig :: Env
defaultConfig =
  Env
    { hconf = "./hconf.yaml",
      hie = "./hie.yaml",
      stack = "./stack.yaml",
      silence = False
    }
