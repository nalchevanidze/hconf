{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.PkgDir
  ( PkgDir,
    pkgDir,
    explore,
    packageFile,
    cabalFile,
  )
where

import Data.Text (intercalate, pack, unpack)
import Relude hiding (Undefined, intercalate)
import System.FilePath.Glob (glob)
import System.FilePath.Posix (joinPath, normalise)

data PkgDir = PkgDir
  { root :: Maybe FilePath,
    dirName :: Text
  }

pkgDir :: Maybe FilePath -> [Text] -> PkgDir
pkgDir dir xs = PkgDir dir (intercalate "-" xs)

resolve :: [FilePath] -> PkgDir -> FilePath
resolve xs PkgDir {..} = normalise (joinPath (maybeToList root <> (toString dirName : xs)))

pkgFile :: FilePath -> PkgDir -> FilePath
pkgFile f = resolve [f]

instance ToText PkgDir where
  toText = pack . resolve []

explore :: (MonadIO m) => PkgDir -> m [String]
explore x = map normalise <$> liftIO (glob (resolve [] x <> "/**/*.hs"))

packageFile :: PkgDir -> FilePath
packageFile = pkgFile "package.yaml"

cabalFile :: Text -> PkgDir -> String
cabalFile pkgName = pkgFile (unpack pkgName <> ".cabal")
