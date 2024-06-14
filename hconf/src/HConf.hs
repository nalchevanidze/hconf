{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf
  ( Env (..),
    VersionTag (..),
    Parse (..),
    exec,
    Command (..),
    currentVersion,
  )
where

import Data.Version (showVersion)
import HConf.Config.Config (Config (..), updateConfig, updateConfigUpperBounds)
import HConf.Config.ConfigT (HCEnv (..), run, runTask, save)
import HConf.Config.Tag (VersionTag (..))
import HConf.Core.Env (Env (..))
import HConf.Format (formatWith)
import HConf.Hie (genHie)
import HConf.Stack.Config (setupStack)
import HConf.Stack.Package (checkPackages)
import HConf.Utils.Class (Parse (..))
import qualified Paths_hconf as CLI
import Relude hiding (fix)

format :: Bool -> Env -> IO ()
format fix = runTask "format" $ formatWith fix

upperBounds :: Env -> IO ()
upperBounds =
  runTask "upper-bounds"
    $ asks config
    >>= updateConfigUpperBounds
    >>= save

setup :: Maybe VersionTag -> Env -> IO ()
setup v = runTask "setup" $ do
  setupStack (fromMaybe Latest v)
  genHie
  checkPackages

updateVersion :: Bool -> Env -> IO ()
updateVersion isBreaking = runTask "next" $ (asks config <&> updateConfig isBreaking) >>= save

getVersion :: Env -> IO ()
getVersion = run (Just . version <$> asks config)

data Command
  = Setup (Maybe VersionTag)
  | Next Bool
  | UpperBounds
  | About
  | CurrentVersion
  | Format Bool
  deriving (Show)

currentVersion :: String
currentVersion = showVersion CLI.version

exec :: Env -> Command -> IO ()
exec _ About = putStrLn $ "Stack Config CLI, version " <> currentVersion
exec e (Setup v) = setup v e
exec e (Next isBreaking) = updateVersion isBreaking e
exec e UpperBounds = upperBounds e
exec e CurrentVersion = getVersion e
exec e (Format fix) = format (not fix) e
