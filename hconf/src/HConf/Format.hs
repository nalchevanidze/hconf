{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Format (format) where

import Data.Text (unpack)
import qualified Data.Text.IO.Utf8 as T.Utf8
import HConf.Config.ConfigT (ConfigT, packages)
import HConf.Utils.Log (label, task)
import Ormolu
  ( ColorMode (..),
    Config (..),
    defaultConfig,
    detectSourceType,
    ormolu,
    withPrettyOrmoluExceptions,
  )
import Ormolu.Diff.Text (diffText, printTextDiff)
import Ormolu.Terminal (runTerm)
import Relude hiding (exitWith)
import System.Exit (ExitCode (..))
import System.FilePath (normalise)
import System.FilePath.Glob (glob)

toPattern :: Text -> String
toPattern x = unpack x <> "/**/*.hs"

format :: ConfigT ()
format = label "ormolu"
  $ task "format"
  $ do
    fs <- map toPattern <$> packages
    files <- concat <$> liftIO (traverse glob fs)
    liftIO $ formatFiles True files

formatFiles :: Bool -> [FilePath] -> IO ()
formatFiles fix files = do
  case files of
    [] -> pure ()
    [x] -> formatOne fix x $> ()
    xs -> do
      let selectFailure = \case
            ExitSuccess -> Nothing
            ExitFailure n -> Just n
      errorCodes <-
        mapMaybe selectFailure <$> mapM (formatOne fix) (sort xs)
      if null errorCodes then pure () else fail "Error"

colorMode :: ColorMode
colorMode = Always

formatOne ::
  Bool ->
  FilePath ->
  IO ExitCode
formatOne fix path = withPrettyOrmoluExceptions colorMode result
  where
    result
      | fix = do
          originalInput <- T.Utf8.readFile inputFile
          formattedInput <-
            ormolu config inputFile originalInput
          when (formattedInput /= originalInput)
            $ T.Utf8.writeFile inputFile formattedInput
          return ExitSuccess
      | otherwise = do
          originalInput <- T.Utf8.readFile inputFile
          formattedInput <-
            ormolu config inputFile originalInput
          handleDiff originalInput formattedInput inputFile
    inputFile = normalise path
    config =
      defaultConfig
        { cfgCheckIdempotence = True,
          cfgColorMode = colorMode,
          cfgSourceType = detectSourceType inputFile
        }

handleDiff :: Text -> Text -> FilePath -> IO ExitCode
handleDiff originalInput formattedInput fileRepr =
  case diffText originalInput formattedInput fileRepr of
    Nothing -> return ExitSuccess
    Just diff -> do
      runTerm (printTextDiff diff) colorMode stderr
      return (ExitFailure 100)
