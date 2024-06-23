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

import Data.Aeson (FromJSON (..), ToJSON (toJSON))
import Data.Aeson.Types (Value (..))
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

parseDir :: FilePath -> PkgDir
parseDir x = case splitFileName x of
  (dir, name)
    | dir == "./" -> PkgDir Nothing (fromString name)
    | otherwise -> PkgDir (Just dir) (fromString name)

instance FromJSON PkgDir where
  parseJSON = withString "PkgDir" (pure . parseDir . toString)

instance ToJSON PkgDir where
  toJSON = String . fromString . resolve []
