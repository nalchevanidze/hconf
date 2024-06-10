{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module HConf.Format (format) where

import Control.Monad
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text.IO.Utf8 as T.Utf8
import Ormolu
import Ormolu.Diff.Text (diffText, printTextDiff)
import Ormolu.Fixity
import Ormolu.Terminal
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

rawConfig :: (Config RegionIndices)
rawConfig =
  Config
    []
    (FixityOverrides Map.empty)
    (ModuleReexports Map.empty)
    Set.empty
    False
    False
    True -- "check-idempotence"
    ModuleSource
    Auto
    (RegionIndices Nothing Nothing)

-- | Format a single input.
formatOne ::
  -- | Mode of operation
  Mode ->
  FilePath ->
  IO ExitCode
formatOne mode mpath =
  withPrettyOrmoluExceptions (cfgColorMode rawConfig) $ do
    case mode of
      InPlace -> do
        originalInput <- T.Utf8.readFile inputFile
        formattedInput <-
          ormolu config inputFile originalInput
        when (formattedInput /= originalInput) $
          T.Utf8.writeFile inputFile formattedInput
        return ExitSuccess
      Check -> do
        -- ormoluFile is not used because we need originalInput
        originalInput <- T.Utf8.readFile inputFile
        formattedInput <-
          ormolu config inputFile originalInput
        handleDiff originalInput formattedInput inputFile
  where
    inputFile = FP.normalise mpath
    config =
      refineConfig
        (fromMaybe ModuleSource (Just (detectSourceType inputFile)))
        Nothing
        (Just (cfgFixityOverrides rawConfig))
        (Just (cfgModuleReexports rawConfig))
        ( rawConfig
            { cfgFixityOverrides = maybe defaultFixityOverrides fst Nothing,
              cfgModuleReexports = maybe defaultModuleReexports snd Nothing
            }
        )
    handleDiff originalInput formattedInput fileRepr =
      case diffText originalInput formattedInput fileRepr of
        Nothing -> return ExitSuccess
        Just diff -> do
          runTerm (printTextDiff diff) (cfgColorMode rawConfig) stderr
          -- 100 is different to all the other exit code that are emitted
          -- either from an 'OrmoluException' or from 'error' and
          -- 'notImplemented'.
          return (ExitFailure 100)
