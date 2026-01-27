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
import HMM.Config.ConfigT (HCEnv (..), run)
import HMM.Config.Tag (Tag (Latest))
import HMM.Core.Env (Env (..), defaultConfig)
import HMM.Format (format)
import HMM.Hie (syncHie)
import HMM.Stack.Config (syncStackYaml)
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

exec :: Command -> Env -> IO ()
-- commands that must do build validation and require https requests
exec Use {tag} = run False (Just "use") Nothing $ syncStackYaml tag
exec UpdateDeps = run False (Just "update deps") (Just updateConfig) syncPackages
exec Version {bump = Just bump} = run True (Just "version") (Just (pure . bumpVersion bump)) syncPackages
-- commands that can run in fast mode without build validation
exec Sync = run True (Just "sync") Nothing (syncHie *> syncPackages)
exec Version {bump = Nothing} = run True Nothing Nothing (version <$> asks config)
exec Format {check} = run True (Just "format") Nothing (format check)