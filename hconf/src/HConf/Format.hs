{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Format (format) where

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
import Relude hiding (exitWith)
import System.Exit (ExitCode (..))
import System.FilePath (normalise)
import System.FilePath.Glob (glob)

format :: (MonadIO m) => [FilePath] -> m ()
format patterns = liftIO $ do
  files <- concat <$> traverse glob patterns
  let mode = InPlace
  case files of
    [] -> pure ()
    [x] -> formatOne mode x $> ()
    xs -> do
      let selectFailure = \case
            ExitSuccess -> Nothing
            ExitFailure n -> Just n
      errorCodes <-
        mapMaybe selectFailure <$> mapM (formatOne mode) (sort xs)
      if null errorCodes then pure () else fail "Error"

-- format :: MonadIO m =>[FilePath] -> m ()
-- format files = liftIO $ do
--   ls <- glob "hconf/**/**.hs"
--   print ls
--   let mode = InPlace
--   exitCode <- case files of
--     [] -> pure ExitSuccess
--     [x] -> formatOne mode x
--     xs -> do
--       let selectFailure = \case
--             ExitSuccess -> Nothing
--             ExitFailure n -> Just n
--       errorCodes <-
--         mapMaybe selectFailure <$> mapM (formatOne mode) (sort xs)
--       return $
--         if null errorCodes
--           then ExitSuccess
--           else
--             ExitFailure $
--               if all (== 100) errorCodes
--                 then 100
--                 else 102
--   exitWith exitCode

data Mode = InPlace | Check
  deriving (Eq, Show)

colorMode :: ColorMode
colorMode = Auto

formatOne ::
  Mode ->
  FilePath ->
  IO ExitCode
formatOne mode path = withPrettyOrmoluExceptions colorMode (result mode)
  where
    result InPlace = do
      originalInput <- T.Utf8.readFile inputFile
      formattedInput <-
        ormolu config inputFile originalInput
      when (formattedInput /= originalInput)
        $ T.Utf8.writeFile inputFile formattedInput
      return ExitSuccess
    result Check = do
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
    handleDiff originalInput formattedInput fileRepr =
      case diffText originalInput formattedInput fileRepr of
        Nothing -> return ExitSuccess
        Just diff -> do
          runTerm (printTextDiff diff) colorMode stderr
          return (ExitFailure 100)
