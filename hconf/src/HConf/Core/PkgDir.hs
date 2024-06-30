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
    pkgFile,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Types (Value (..))
import Data.List (dropWhileEnd)
import Data.Text (intercalate)
import HConf.Utils.Core (Msg (..), withString)
import Relude hiding (Undefined, intercalate)
import System.FilePath.Glob (glob)
import System.FilePath.Posix
  ( joinPath,
    normalise,
    splitFileName,
  )

data PkgDir = PkgDir
  { root :: Maybe FilePath,
    dirName :: Text
  }
  deriving (Show, Eq)

instance Msg PkgDir where
  msg = msg . resolve []

pkgDir :: Maybe FilePath -> [Text] -> PkgDir
pkgDir dir xs = PkgDir dir (intercalate "-" xs)

resolve :: [FilePath] -> PkgDir -> FilePath
resolve xs PkgDir {..} = normalise (joinPath (maybeToList root <> (toString dirName : xs)))

pkgFile :: FilePath -> PkgDir -> FilePath
pkgFile f = resolve [f]

instance ToText PkgDir where
  toText = fromString . resolve []

explore :: (MonadIO m) => PkgDir -> m [String]
explore x = map normalise <$> liftIO (glob (resolve [] x <> "/**/*.hs"))

packageFile :: PkgDir -> FilePath
packageFile = pkgFile "package.yaml"

cabalFile :: Text -> PkgDir -> String
cabalFile name = pkgFile (toString name <> ".cabal")

resolveDir :: String -> Maybe String
resolveDir "./" = Nothing
resolveDir name = Just $ dropWhileEnd (/= '/') name

parseDir :: FilePath -> PkgDir
parseDir x =
  let (dir, name) = splitFileName x
   in PkgDir (resolveDir dir) (fromString name)

instance FromJSON PkgDir where
  parseJSON = withString "PkgDir" (pure . parseDir . toString)

instance ToJSON PkgDir where
  toJSON = String . fromString . resolve []
