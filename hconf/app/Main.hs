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
import HConf
import HConf (Command (..), Env (..), currentVersion, exec)
import Relude hiding (ByteString, fix)

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
  | otherwise = exec env operations
