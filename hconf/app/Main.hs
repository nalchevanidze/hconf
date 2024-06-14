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
    Options (..),
    parseApp,
  )
import HConf (Env (..), currentVersion, exec)
import Relude hiding (ByteString, fix)

main :: IO ()
main = parseApp >>= runApp
  where
    runApp App {..}
      | version options = putStrLn currentVersion
      | otherwise =
          exec
            ( Env
                { hconf = "./hconf.yaml",
                  hie = "./hie.yaml",
                  stack = "./stack.yaml",
                  silence = False
                }
            )
            operations
