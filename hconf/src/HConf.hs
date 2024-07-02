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
import HConf.Config.Config (Config (..), nextVersionConfig, updateConfigUpperBounds)
import HConf.Config.ConfigT (HCEnv (..), run, runTask, save)
import HConf.Config.Tag (Tag (Latest))
import HConf.Core.Env (Env (..), defaultConfig)
import HConf.Format (format)
import HConf.Hie (genHie)
import HConf.Stack.Config (setupStack)
import HConf.Stack.Package (checkPackages)
import HConf.Utils.Class (Parse (..))
import qualified Paths_hconf as CLI
import Relude hiding (fix)

data Command
  = Setup {tag :: Maybe Tag}
  | Next {isBreaking :: Bool}
  | Update
  | About
  | Version
  | Format {check :: Bool}
  deriving (Show)

currentVersion :: String
currentVersion = showVersion CLI.version

exec :: Command -> Env -> IO ()
exec About = const $ putStrLn $ "Stack Config CLI, version " <> currentVersion
exec Setup {tag} =
  runTask "setup" $ do
    setupStack (fromMaybe Latest tag)
    genHie
    checkPackages
exec Next {isBreaking} =
  runTask "next"
    $ (asks config <&> nextVersionConfig isBreaking)
    >>= save
exec Update =
  runTask "update"
    $ asks config
    >>= updateConfigUpperBounds
    >>= save
exec Version = run (Just . version <$> asks config)
exec Format {check} = runTask "format" $ format check
