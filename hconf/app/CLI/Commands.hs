{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Commands
  ( Options (..),
    App (..),
    Command (..),
    parseApp,
  )
where

import HConf (Command (..), Parse (parse), VersionTag)
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
import qualified Options.Applicative as OA
import Options.Applicative.Builder (str)
import Relude hiding (ByteString)

data App = App
  { operations :: Command,
    options :: Options
  }
  deriving (Show)

data Options = Options
  { version :: Bool,
    silence :: Bool
  }
  deriving (Show)

parseCommand :: Parser Command
parseCommand =
  parsers
    [ ("setup", "builds Haskell code from GQL source", Setup <$> optional parseVersion),
      ("about", "api information", pure About),
      ("update", "check/fix upper bounds for dependencies", pure UpperBounds),
      ("next", "next release", Next <$> switch (long "breaking" <> short 'b')),
      ("version", "get current version", pure CurrentVersion),
      ("format", "format files in projects", Format <$> switch (long "check" <> short 'c'))
    ]

parsers :: [(String, String, Parser Command)] -> Parser Command
parsers =
  subparser
    . mconcat
    . map
      ( \(name, desc, value) ->
          command name (info (helper <*> value) (fullDesc <> progDesc desc))
      )

parseVersion :: Parser VersionTag
parseVersion = argument (str >>= parse) (metavar "version" <> help "version tag")

parseApp :: IO App
parseApp =
  customExecParser
    (prefs showHelpOnError)
    (info (helper <*> (App <$> parseCommand <*> parseOptions)) description)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch (long "version" <> short 'v' <> help "show Version number")
    <*> switch (long "silence" <> short 's' <> help "silent")

description :: OA.InfoMod a
description = fullDesc <> progDesc "HConf CLI - manage multiple haskell projects"
