{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.PkgDir
  ( PkgDir (..),
    toPkgName,
    pkgFile,
  )
where

import Data.Text (pack)
import Relude hiding (Undefined, intercalate)
import System.FilePath.Posix (joinPath, normalise)

data PkgDir = PkgDir {pkgRoot :: Maybe FilePath, pkgName :: Text}

toPkgName :: Maybe FilePath -> Text -> PkgDir
toPkgName = PkgDir

resolve :: PkgDir -> [FilePath] -> FilePath
resolve (PkgDir d n) xs = normalise (joinPath (maybeToList d <> (toString n : xs)))

pkgFile :: PkgDir -> FilePath -> FilePath
pkgFile pkg f = resolve pkg [f]

instance ToString PkgDir where
  toString (PkgDir d n) = normalise (joinPath (maybeToList d <> [toString n]))

instance ToText PkgDir where
  toText = pack . toString
