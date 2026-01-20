{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Format (format) where

import HMM.Core.PkgDir (explore)
import HMM.Utils.Class (HConfIO)
import HMM.Utils.Core (isSuccess, throwError)
import HMM.Utils.FromConf (ReadConf, readList)
import HMM.Utils.Log (task)
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
