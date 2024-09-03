{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.PkgDir
  ( PkgDir,
    PkgDirs,
    genPkgDir,
    explore,
    packageFile,
    cabalFile,
    pkgFile,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Types (Value (..))
import Data.List (stripPrefix)
import Data.Text (intercalate)
import HConf.Utils.Core (Msg (..), Name, PkgName, withString)
import Relude hiding (Undefined, intercalate)
import System.FilePath.Glob (glob)
import System.FilePath.Posix
  ( joinPath,
    normalise,
    splitDirectories,
    splitFileName,
  )

type PkgDirs = [PkgDir]

data PkgDir = PkgDir
  { root :: Maybe FilePath,
    dirName :: Name
  }
  deriving (Show, Eq)

instance Msg PkgDir where
  msg = msg . resolve []

genPkgDir :: Maybe FilePath -> [Name] -> PkgDir
genPkgDir dir = PkgDir (dir >>= resolveDir) . intercalate "-"

resolve :: [FilePath] -> PkgDir -> FilePath
resolve xs PkgDir {..} = normalise (joinPath (maybeToList root <> (toString dirName : xs)))

pkgFile :: FilePath -> PkgDir -> FilePath
pkgFile f = resolve [f]

instance ToString PkgDir where
  toString = resolve []

explore :: (MonadIO m) => PkgDir -> m [String]
explore x = map normalise <$> liftIO (glob (resolve [] x <> "/**/*.hs"))

packageFile :: PkgDir -> FilePath
packageFile = pkgFile "package.yaml"

cabalFile :: PkgName -> PkgDir -> String
cabalFile name = pkgFile (toString name <> ".cabal")

resolveDir :: String -> Maybe String
resolveDir "./" = Nothing
resolveDir name =
  Just
    $ joinPath
    $ splitDirectories
    $ fromMaybe name (stripPrefix "./" name)

parseDir :: FilePath -> PkgDir
parseDir x =
  let (dir, name) = second fromString (splitFileName x)
   in PkgDir (resolveDir dir) name

instance FromJSON PkgDir where
  parseJSON = withString "PkgDir" (pure . parseDir . toString)

instance ToJSON PkgDir where
  toJSON = String . fromString . resolve []
