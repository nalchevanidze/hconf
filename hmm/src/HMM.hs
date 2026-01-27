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
import HMM.Config.Bump (Bump (..))
import HMM.Config.Config (Config (..), bumpVersion, updateConfig)
import HMM.Config.ConfigT (ConfigT, HCEnv (..), localConfig, run, runTask)
import HMM.Config.Tag (Tag (Latest))
import HMM.Core.Env (Env (..), defaultConfig)
import HMM.Format (format)
import HMM.Hie (syncHie)
import HMM.Stack.Config (setupStack)
import HMM.Stack.Package (syncPackages)
import HMM.Utils.Class (Parse (..))
import qualified Paths_hmm as CLI
import Relude hiding (fix)

data Command
  = Use {tag :: Maybe Tag}
  | Sync
  | Version {bump :: Maybe Bump}
  | UpdateDeps
  | Format {check :: Bool}
  deriving (Show)

currentVersion :: String
currentVersion = showVersion CLI.version

sync :: ConfigT ()
sync = syncHie *> syncPackages

exec :: Command -> Env -> IO ()
-- commands that must do build validation and require https requests
exec Use {tag} = runTask False "use" $ setupStack (fromMaybe Latest tag)
exec UpdateDeps = runTask False "update deps" (localConfig updateConfig)
-- commands that can run in fast mode without build validation
exec Sync = runTask True "sync" sync
exec Version {bump = Just bump} = runTask True "version" (localConfig (pure . bumpVersion bump) >> sync)
exec Version {bump = Nothing} = run True (Just . version <$> asks config)
exec Format {check} = runTask True "format" $ format check
