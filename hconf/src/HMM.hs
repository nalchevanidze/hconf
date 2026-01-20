{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM
  ( Env (..),
    Tag (..),
    Parse (..),
    exec,
    Command (..),
    currentVersion,
    defaultConfig,
    Bump (..),
  )
where

import Data.Version (showVersion)
import HConf.Config.Bump (Bump (..))
import HConf.Config.Config (Config (..), nextRelease, updateConfig)
import HConf.Config.ConfigT (HCEnv (..), run, runTask, save)
import HConf.Config.Tag (Tag (Latest))
import HConf.Core.Env (Env (..), defaultConfig)
import HConf.Format (format)
import HConf.Hie (genHie)
import HConf.Stack.Config (setupStack)
import HConf.Stack.Package (checkPackages)
import HConf.Utils.Class (Parse (..))
import qualified Paths_hmm as CLI
import Relude hiding (fix)

data Command
  = Setup {tag :: Maybe Tag}
  | Version {bump :: Maybe Bump}
  | UpdateDeps
  | Format {check :: Bool}
  deriving (Show)

currentVersion :: String
currentVersion = showVersion CLI.version

exec :: Command -> Env -> IO ()
exec Setup {tag} =
  runTask "setup" $ do
    setupStack (fromMaybe Latest tag)
    genHie
    checkPackages
exec Version {bump = Nothing} =
  run (Just . version <$> asks config)
exec Version {bump = Just bump} =
  runTask "version"
    $ (asks config <&> nextRelease bump)
    >>= save
exec UpdateDeps =
  runTask "update-deps"
    $ asks config
    >>= updateConfig
    >>= save
exec Format {check} =
  runTask "format"
    $ format check
