{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Format (format) where

import HConf.Core.PkgDir (explore)
import HConf.Utils.Class (HConfIO)
import HConf.Utils.Core (isSuccess, throwError)
import HConf.Utils.FromConf (ReadConf, readList)
import HConf.Utils.Log (task)
import Ormolu
  ( ColorMode (..),
    Config (..),
    defaultConfig,
    ormolu,
    withPrettyOrmoluExceptions,
  )
import Ormolu.Diff.Text (TextDiff, diffText, printTextDiff)
import Ormolu.Terminal (runTerm)
import Relude hiding (exitWith, fix)
import System.Exit (ExitCode (..))

format :: (ReadConf m ()) => Bool -> m ()
format check = task "ormolu" $ do
  files <- sort . concat <$> (readList >>= traverse explore)
  success <- all isSuccess <$> mapM (formatFile check) files
  unless success (throwError "Error")

formatFile :: (HConfIO m) => Bool -> FilePath -> m ExitCode
formatFile check path = liftIO $ withPrettyOrmoluExceptions Always $ do
  original <- readFileText path
  formatted <- formatter path original
  handle original formatted
  where
    handle original formatted
      | not check = when (formatted /= original) (writeFileText path formatted) $> ExitSuccess
      | otherwise = handleDiff (diffText original formatted path)

formatter :: FilePath -> Text -> IO Text
formatter = ormolu defaultConfig {cfgCheckIdempotence = True, cfgColorMode = Always}

handleDiff :: Maybe TextDiff -> IO ExitCode
handleDiff = maybe (pure ExitSuccess) (\diff -> runTerm (printTextDiff diff) Always stderr $> ExitFailure 100)
