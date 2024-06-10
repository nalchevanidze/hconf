{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module HConf.Format (main) where

import Control.Monad
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text.IO.Utf8 as T.Utf8
-- import Distribution.ModuleName (ModuleName)
-- import Distribution.Types.PackageName (PackageName)
-- import Language.Haskell.TH.Env (envQ)
import Ormolu
import Ormolu.Diff.Text (diffText, printTextDiff)
import Ormolu.Fixity
import Ormolu.Terminal
import System.Directory
import System.Exit (ExitCode (..), exitWith)
import qualified System.FilePath as FP
import System.IO (hPutStrLn, stderr)

-- | Entry point of the program.
main :: [FilePath] -> IO ()
main files = do
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
    let getCabalInfoForSourceFile' sourceFile = do
          cabalSearchResult <- getCabalInfoForSourceFile sourceFile
          let debugEnabled = cfgDebug rawConfig
          case cabalSearchResult of
            CabalNotFound -> do
              when debugEnabled $
                hPutStrLn stderr $
                  "Could not find a .cabal file for " <> sourceFile
              return Nothing
            CabalDidNotMention cabalInfo -> do
              when debugEnabled $ do
                relativeCabalFile <-
                  makeRelativeToCurrentDirectory (ciCabalFilePath cabalInfo)
                hPutStrLn stderr $
                  "Found .cabal file "
                    <> relativeCabalFile
                    <> ", but it did not mention "
                    <> sourceFile
              return (Just cabalInfo)
            CabalFound cabalInfo -> return (Just cabalInfo)
        getDotOrmoluForSourceFile' _ = return Nothing
    let inputFile = FP.normalise mpath
    mdotOrmolu <- getDotOrmoluForSourceFile' inputFile
    config <-
      patchConfig
        (Just (detectSourceType inputFile))
        Nothing
        mdotOrmolu
    case mode of
      InPlace -> do
        -- ormoluFile is not used because we need originalInput
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
    patchConfig mdetectedSourceType mcabalInfo mdotOrmolu = do
      let sourceType = fromMaybe ModuleSource mdetectedSourceType
      return $
        refineConfig
          sourceType
          mcabalInfo
          (Just (cfgFixityOverrides rawConfig))
          (Just (cfgModuleReexports rawConfig))
          ( rawConfig
              { cfgFixityOverrides = maybe defaultFixityOverrides fst mdotOrmolu,
                cfgModuleReexports = maybe defaultModuleReexports snd mdotOrmolu
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
