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
import HMM.Config.ConfigT (HCEnv (..), run, runTask, save)
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

exec :: Command -> Env -> IO ()
exec Use {tag} = runTask False "use" $ setupStack (fromMaybe Latest tag)
exec Sync = runTask False "sync" $ syncHie *> syncPackages
exec Version {bump = Just bump} =
  runTask False "version"
    $ (asks config <&> bumpVersion bump)
    >>= save
exec UpdateDeps =
  runTask False "update deps"
    $ asks config
    >>= updateConfig
    >>= save
-- commands that does not need validation and we can run in fast mode
exec Version {bump = Nothing} = run True (Just . version <$> asks config)
exec Format {check} = runTask True "format" $ format check
