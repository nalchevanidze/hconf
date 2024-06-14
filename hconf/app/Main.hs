{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import HConf
  ( Command (..),
    Parse (parse),
    Tag,
    currentVersion,
    defaultConfig,
    exec,
  )
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
        (fullDesc <> progDesc "HConf CLI - manage multiple haskell projects")
    )

class CLIType a where
  cliType :: Parser a

instance CLIType Tag where
  cliType = argument (str >>= parse) (metavar "version" <> help "version tag")

instance CLIType Command where
  cliType =
    commands
      [ ("setup", "builds Haskell code from GQL source", Setup <$> optional cliType),
        ("about", "api information", pure About),
        ("update", "check/fix upper bounds for dependencies", pure UpperBounds),
        ("next", "next release", Next <$> switch (long "breaking" <> short 'b')),
        ("version", "get current version", pure CurrentVersion),
        ("format", "format files in projects", Format <$> switch (long "check" <> short 'c'))
      ]

data Options = Options
  { optVersion :: Bool,
    optSilence :: Bool
  }
  deriving (Show)

instance CLIType Options where
  cliType =
    Options
      <$> flag 'v' "version" "show Version number"
      <*> flag 's' "silence" "silent"

main :: IO ()
main = run ((,) <$> cliType <*> cliType) >>= runApp
  where
    runApp (cmd, ops)
      | optVersion ops = putStrLn currentVersion
      | otherwise = exec defaultConfig cmd
