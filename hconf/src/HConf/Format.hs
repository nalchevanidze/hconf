{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module HConf.Format (main) where

import Control.Exception (throwIO)
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
-- import System.FilePath qualified as FP
import System.IO (hPutStrLn, stderr)

-- | Entry point of the program.
main :: IO ()
main = do
  Opts {..} <- optsParser
  let formatOne' =
        formatOne
          optConfigFileOpts
          optMode
          Nothing
          optConfig
  exitCode <- case optInputFiles of
    [] -> formatOne' Nothing
    ["-"] -> formatOne' Nothing
    [x] -> formatOne' (Just x)
    xs -> do
      let selectFailure = \case
            ExitSuccess -> Nothing
            ExitFailure n -> Just n
      errorCodes <-
        mapMaybe selectFailure <$> mapM (formatOne' . Just) (sort xs)
      return $
        if null errorCodes
          then ExitSuccess
          else
            ExitFailure $
              if all (== 100) errorCodes
                then 100
                else 102
  exitWith exitCode

-- | Format a single input.
formatOne ::
  -- | How to use .cabal files
  ConfigFileOpts ->
  -- | Mode of operation
  Mode ->
  -- | The 'SourceType' requested by the user
  Maybe SourceType ->
  -- | Configuration
  Config RegionIndices ->
  -- | File to format or stdin as 'Nothing'
  Maybe FilePath ->
  IO ExitCode
formatOne ConfigFileOpts {..} mode reqSourceType rawConfig mpath =
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
        getDotOrmoluForSourceFile' sourceFile = do
          if optDoNotUseDotOrmolu
            then return Nothing
            else Just <$> getDotOrmoluForSourceFile sourceFile
    case FP.normalise <$> mpath of
      -- input source = STDIN
      Nothing -> do
        mcabalInfo <- case (optStdinInputFile, optDoNotUseCabal) of
          (_, True) -> return Nothing
          (Nothing, False) -> throwIO OrmoluMissingStdinInputFile
          (Just inputFile, False) -> getCabalInfoForSourceFile' inputFile
        mdotOrmolu <- case optStdinInputFile of
          Nothing -> return Nothing
          Just inputFile -> getDotOrmoluForSourceFile' inputFile
        config <- patchConfig Nothing mcabalInfo mdotOrmolu
        case mode of
          Stdout -> do
            ormoluStdin config >>= T.Utf8.putStr
            return ExitSuccess
          InPlace -> do
            hPutStrLn
              stderr
              "In place editing is not supported when input comes from stdin."
            -- 101 is different from all the other exit codes we already use.
            return (ExitFailure 101)
          Check -> do
            -- ormoluStdin is not used because we need the originalInput
            originalInput <- T.Utf8.getContents
            let stdinRepr = "<stdin>"
            formattedInput <-
              ormolu config stdinRepr originalInput
            handleDiff originalInput formattedInput stdinRepr
      -- input source = a file
      Just inputFile -> do
        mcabalInfo <-
          if optDoNotUseCabal
            then return Nothing
            else getCabalInfoForSourceFile' inputFile
        mdotOrmolu <- getDotOrmoluForSourceFile' inputFile
        config <-
          patchConfig
            (Just (detectSourceType inputFile))
            mcabalInfo
            mdotOrmolu
        case mode of
          Stdout -> do
            ormoluFile config inputFile >>= T.Utf8.putStr
            return ExitSuccess
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
      let sourceType =
            fromMaybe
              ModuleSource
              (reqSourceType <|> mdetectedSourceType)
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

----------------------------------------------------------------------------
-- Command line options parsing

-- | All command line options.
data Opts = Opts
  { -- | Mode of operation
    optMode :: !Mode,
    -- | Ormolu 'Config'
    optConfig :: !(Config RegionIndices),
    -- | Options related to info extracted from files
    optConfigFileOpts :: ConfigFileOpts,
    -- | Source type option, where 'Nothing' means autodetection
    optSourceType :: !(Maybe SourceType),
    -- | Haskell source files to format or stdin (when the list is empty)
    optInputFiles :: ![FilePath]
  }

-- | Mode of operation.
data Mode
  = -- | Output formatted source code to stdout
    Stdout
  | -- | Overwrite original file
    InPlace
  | -- | Exit with non-zero status code if
    -- source is not already formatted
    Check
  deriving (Eq, Show)

-- | Options related to configuration stored in the file system.
data ConfigFileOpts = ConfigFileOpts
  { -- | DO NOT extract default-extensions and dependencies from .cabal files
    optDoNotUseCabal :: Bool,
    -- | DO NOT look for @.ormolu@ files
    optDoNotUseDotOrmolu :: Bool,
    -- | Optional path to a file which will be used to find a .cabal file
    -- when using input from stdin
    optStdinInputFile :: Maybe FilePath
  }
  deriving (Show)


parseMode :: Bool -> Mode
parseMode True = InPlace
parseMode False = Check

optsParser :: m Opts
optsParser =
  Opts InPlace configParser configFileOptsParser Nothing
    <$> (many . strArgument . mconcat)
      [ metavar "FILE",
        help "Haskell source files to format or stdin (the default)"
      ]

configFileOptsParser :: ConfigFileOpts
configFileOptsParser = ConfigFileOpts False False Nothing

configParser :: (Config RegionIndices)
configParser =
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
