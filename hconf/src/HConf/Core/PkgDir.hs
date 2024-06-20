{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HConf.Core.PkgDir
  ( PkgDir,
    toPkgName,
    pkgFile,
    explore,
  )
where

import Data.Text (intercalate, pack)
import Relude hiding (Undefined, intercalate)
import System.FilePath.Glob (glob)
import System.FilePath.Posix (joinPath, normalise)

data PkgDir = PkgDir
  { root :: Maybe FilePath,
    name :: Text
  }

toPkgName :: Maybe FilePath -> [Text] -> PkgDir
toPkgName dir xs = PkgDir dir (intercalate "-" xs)

resolve :: [FilePath] -> PkgDir -> FilePath
resolve xs PkgDir {..} = normalise (joinPath (maybeToList root <> (toString name : xs)))

pkgFile :: FilePath -> PkgDir -> FilePath
pkgFile f = resolve [f]

instance ToText PkgDir where
  toText = pack . resolve []

explore :: (MonadIO m) => PkgDir -> m [String]
explore x = map normalise <$> liftIO (glob (resolve [] x <> "/**/*.hs"))
