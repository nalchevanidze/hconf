{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module HConf.Format (format) where

import Control.Monad
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text.IO.Utf8 as T.Utf8
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
import System.Exit (ExitCode (..), exitWith)
import qualified System.FilePath as FP
import System.IO (stderr)

format :: [FilePath] -> IO ()
format files = do
  let mode = InPlace
  exitCode <- case files of
    [] -> pure ExitSuccess
    [x] -> formatOne mode x
    xs -> do
      let selectFailure = \case
            ExitSuccess -> Nothing
            ExitFailure n -> Just n
      errorCodes <-
        mapMaybe selectFailure <$> mapM (formatOne mode) (sort xs)
      return $
        if null errorCodes
          then ExitSuccess
          else
            ExitFailure $
              if all (== 100) errorCodes
                then 100
                else 102
  exitWith exitCode

data Mode = InPlace | Check
  deriving (Eq, Show)

colorMode :: ColorMode
colorMode = Auto

-- | Format a single input.
formatOne ::
  -- | Mode of operation
  Mode ->
  FilePath ->
  IO ExitCode
formatOne mode mpath = withPrettyOrmoluExceptions colorMode (result mode)
  where
    result InPlace = do
      originalInput <- T.Utf8.readFile inputFile
      formattedInput <-
        ormolu config inputFile originalInput
      when (formattedInput /= originalInput) $
        T.Utf8.writeFile inputFile formattedInput
      return ExitSuccess
    result Check = do
      originalInput <- T.Utf8.readFile inputFile
      formattedInput <-
        ormolu config inputFile originalInput
      handleDiff originalInput formattedInput inputFile
    inputFile = FP.normalise mpath
    config =
      defaultConfig
        { cfgCheckIdempotence = True,
          cfgColorMode = Auto,
          cfgSourceType = detectSourceType inputFile
        }
    handleDiff originalInput formattedInput fileRepr =
      case diffText originalInput formattedInput fileRepr of
        Nothing -> return ExitSuccess
        Just diff -> do
          runTerm (printTextDiff diff) colorMode stderr
          -- 100 is different to all the other exit code that are emitted
          -- either from an 'OrmoluException' or from 'error' and
          -- 'notImplemented'.
          return (ExitFailure 100)
