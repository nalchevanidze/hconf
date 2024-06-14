{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import CLI.Commands
  ( App (..),
    Command (..),
    Options (..),
    parseCLI,
  )
import Data.Version (showVersion)
import HConf (Env (..), VersionTag (..), format, getVersion, setup, updateVersion, upperBounds)
import qualified Paths_hconf as CLI
import Relude hiding (ByteString, fix)

currentVersion :: String
currentVersion = showVersion CLI.version

main :: IO ()
main = parseCLI >>= runApp

env :: Env
env =
  Env
    { hconf = "./hconf.yaml",
      hie = "./hie.yaml",
      stack = "./stack.yaml",
      silence = False
    }

runApp :: App -> IO ()
runApp App {..}
  | version options = putStrLn currentVersion
  | otherwise = runOperation operations
  where
    runOperation About = putStrLn $ "Stack Config CLI, version " <> currentVersion
    runOperation (Setup version) = setup (fromMaybe Latest version) env
    runOperation (Next isBreaking) = updateVersion isBreaking env
    runOperation UpperBounds = upperBounds env
    runOperation CurrentVersion = getVersion env
    runOperation (Format fix) = format (not fix) env
