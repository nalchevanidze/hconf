{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Format (format) where

import qualified Data.Text.IO.Utf8 as T
import HConf.Core.PkgDir (explore)
import HConf.Utils.Class (FCon, HConfIO, packages)
import HConf.Utils.Core (throwError)
import HConf.Utils.Log (Log, task)
import Ormolu
  ( ColorMode (..),
    Config (..),
    defaultConfig,
    detectSourceType,
    ormolu,
    withPrettyOrmoluExceptions,
  )
import Ormolu.Diff.Text (TextDiff, diffText, printTextDiff)
import Ormolu.Terminal (runTerm)
import Relude hiding (exitWith, fix)
import System.Exit (ExitCode (..))

format :: (FCon m ()) => Bool -> m ()
format check = task "ormolu" $ do
  files <- sort . concat <$> (packages >>= traverse explore)
  errorCodes <- mapMaybe selectFailure <$> mapM (formatFile check) files
  unless (null errorCodes) (throwError "Error")

formatFile :: (HConfIO m) => Bool -> FilePath -> m ExitCode
formatFile check path = liftIO $ withPrettyOrmoluExceptions Always $ do
  original <- T.readFile path
  formatted <- formatter path original
  handle original formatted
  where
    handle original formatted
      | not check = when (formatted /= original) (T.writeFile path formatted) $> ExitSuccess
      | otherwise = handleDiff (diffText original formatted path)

formatter :: FilePath -> Text -> IO Text
formatter path =
  ormolu
    defaultConfig
      { cfgCheckIdempotence = True,
        cfgColorMode = Always,
        cfgSourceType = detectSourceType path
      }
    path

handleDiff :: Maybe TextDiff -> IO ExitCode
handleDiff = maybe (pure ExitSuccess) (\diff -> runTerm (printTextDiff diff) Always stderr $> ExitFailure 100)

selectFailure :: ExitCode -> Maybe Int
selectFailure ExitSuccess = Nothing
selectFailure (ExitFailure n) = Just n
