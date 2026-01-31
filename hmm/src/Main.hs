{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Data.Text (pack)
import HMM
  ( Bump,
    Command (..),
    Env (..),
    Parse (parse),
    Tag,
    currentVersion,
    defaultConfig,
    exec,
  )
import HMM.Utils.Core (Name)
import Options.Applicative
  ( Parser,
    argument,
    command,
    customExecParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    prefs,
    progDesc,
    short,
    showHelpOnError,
    subparser,
    switch,
  )
import Options.Applicative.Builder (str)
import Relude hiding (ByteString, fix)

commands :: [(String, String, Parser a)] -> Parser a
commands =
  subparser
    . mconcat
    . map
      ( \(name, desc, value) ->
          command name (info (helper <*> value) (fullDesc <> progDesc desc))
      )

flag :: Char -> String -> String -> Parser Bool
flag s l h = switch (long l <> short s <> help h)

run :: Parser a -> IO a
run app =
  customExecParser
    (prefs showHelpOnError)
    ( info
        (helper <*> app)
        (fullDesc <> progDesc "HMM CLI - Haskell Monorepo Manager for multi-GHC projects")
    )

class CLIType a where
  cliType :: Parser a

instance CLIType Tag where
  cliType = argument (str >>= parse) (metavar "VERSION" <> help "version tag to use for setup")

instance CLIType Bump where
  cliType = argument (str >>= parse) (metavar "BUMP" <> help "version bump type: major, minor, or patch")

instance CLIType Command where
  cliType =
    commands
      [ ("use", "select a build from hmm.yaml and generate the active build config", Use <$> optional cliType),
        ("sync", "sync package metadata to match hmm.yaml", pure Sync),
        ("version", "show project info, or bump version with: major|minor|patch", Version <$> optional cliType),
        ("update-deps", "check and update dependency version bounds", pure UpdateDeps),
        ("format", "format Haskell source files using Ormolu (use --check to validate only)", Format <$> switch (long "check" <> short 'c' <> help "check formatting without making changes")),
        ("publish", "publish packages to Hackage", Publish <$> optional (argument (pack <$> str) (metavar "NAME" <> help "name of the package group to publish")))
      ]

data Options = Options
  { optVersion :: Bool,
    optQuiet :: Bool
  }
  deriving (Show)

instance CLIType Options where
  cliType =
    Options
      <$> flag 'v' "version" "show HMM version number"
      <*> flag 'q' "quiet" "run quietly with minimal output"

main :: IO ()
main = do
  (ops, cmd) <- run ((,) <$> cliType <*> optional cliType)
  if optVersion ops
    then putStrLn currentVersion
    else case cmd of
      Just c -> exec c (defaultConfig {quiet = optQuiet ops})
      Nothing -> do
        putStrLn "Missing: COMMAND\n\nUse --help for available commands."
        exitFailure
