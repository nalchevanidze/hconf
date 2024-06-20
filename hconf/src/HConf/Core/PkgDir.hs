{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.PkgDir
  ( PkgDir,
    toPkgName,
    pkgFile,
    explore,
  )
where

import Data.Text (pack)
import Relude hiding (Undefined, intercalate)
import System.FilePath.Glob (glob)
import System.FilePath.Posix (joinPath, normalise)

data PkgDir = PkgDir
  { root :: Maybe FilePath,
    name :: Text
  }

toPkgName :: Maybe FilePath -> Text -> PkgDir
toPkgName = PkgDir

resolve :: [FilePath] -> PkgDir -> FilePath
resolve xs PkgDir {..} = normalise (joinPath (maybeToList root <> (toString name : xs)))

pkgFile :: FilePath -> PkgDir -> FilePath
pkgFile f = resolve [f]

instance ToString PkgDir where
  toString = resolve []

instance ToText PkgDir where
  toText = pack . toString

explore :: (MonadIO m) => PkgDir -> m [String]
explore x = map normalise <$> liftIO (glob (pkgFile "/**/*.hs" x))