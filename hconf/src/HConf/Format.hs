{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Format (formatWith) where

import qualified Data.Text.IO.Utf8 as T
import HConf.Utils.Class (FromConf, readPackages)
import HConf.Utils.Core (Name, PkgDir)
import HConf.Utils.Log (Log, label)
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
import Relude hiding (exitWith, fix)
import System.Exit (ExitCode (..))
import System.FilePath (normalise)
import System.FilePath.Glob (glob)

explore :: (Log m, MonadIO m) => PkgDir -> m [String]
explore x = map normalise <$> liftIO (glob (toString x <> "/**/*.hs"))

formatWith :: (Log m, FromConf m [PkgDir]) => Bool -> m ()
formatWith check = label "ormolu" $ do
  files <- sort . concat <$> (readPackages >>= traverse explore)
  errorCodes <- mapMaybe selectFailure <$> mapM (formatFile check) files
  unless (null errorCodes) (fail "Error")

formatFile :: (MonadIO m) => Bool -> FilePath -> m ExitCode
formatFile check path = liftIO $ withPrettyOrmoluExceptions colorMode $ do
  original <- T.readFile path
  formatted <- formatter path original
  handle original formatted
  where
    handle original formatted
      | not check = when (formatted /= original) (T.writeFile path formatted) $> ExitSuccess
      | otherwise = handleDiff original formatted path

formatter :: (MonadIO m) => FilePath -> Text -> m Text
formatter path =
  ormolu
    defaultConfig
      { cfgCheckIdempotence = True,
        cfgColorMode = colorMode,
        cfgSourceType = detectSourceType path
      }
    path

handleDiff :: Text -> Text -> FilePath -> IO ExitCode
handleDiff original formatted path =
  case diffText original formatted path of
    Nothing -> return ExitSuccess
    Just diff -> do
      runTerm (printTextDiff diff) colorMode stderr
      return (ExitFailure 100)

colorMode :: ColorMode
colorMode = Always

selectFailure :: ExitCode -> Maybe Int
selectFailure ExitSuccess = Nothing
selectFailure (ExitFailure n) = Just n
