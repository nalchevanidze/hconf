{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import HConf (Command (..), Env (..), Parse (parse), VersionTag, currentVersion, exec)
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

data Options = Options
  { optVersion :: Bool,
    optSilence :: Bool
  }
  deriving (Show)

run :: Parser a -> IO a
run app =
  customExecParser
    (prefs showHelpOnError)
    ( info
        (helper <*> app)
        (fullDesc <> progDesc "HConf CLI - manage multiple haskell projects")
    )

parsers :: [(String, String, Parser a)] -> Parser a
parsers =
  subparser
    . mconcat
    . map
      ( \(name, desc, value) ->
          command name (info (helper <*> value) (fullDesc <> progDesc desc))
      )

parseVersion :: Parser VersionTag
parseVersion = argument (str >>= parse) (metavar "version" <> help "version tag")

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

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch (long "version" <> short 'v' <> help "show Version number")
    <*> switch (long "silence" <> short 's' <> help "silent")

main :: IO ()
main = run ((,) <$> parseCommand <*> parseOptions) >>= runApp
  where
    runApp (cmd, ops)
      | optVersion ops = putStrLn currentVersion
      | otherwise = do
          exec
            ( Env
                { hconf = "./hconf.yaml",
                  hie = "./hie.yaml",
                  stack = "./stack.yaml",
                  silence = False
                }
            )
            cmd
