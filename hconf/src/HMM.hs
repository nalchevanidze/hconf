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
  )
where

import Data.Version (showVersion)
import HMM.Config.Bump (Bump (..))
import HMM.Config.Config (Config (..), nextRelease, updateConfig)
import HMM.Config.ConfigT (HCEnv (..), run, runTask, save)
import HMM.Config.Tag (Tag (Latest))
import HMM.Core.Env (Env (..), defaultConfig)
import HMM.Format (format)
import HMM.Hie (genHie)
import HMM.Stack.Config (setupStack)
import HMM.Stack.Package (checkPackages)
import HMM.Utils.Class (Parse (..))
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
