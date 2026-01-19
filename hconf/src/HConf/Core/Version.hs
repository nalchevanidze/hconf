{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Version
  ( nextVersion,
    dropPatch,
    Version,
    readVersion,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import GHC.Show (Show (..))
import HConf.Config.Bump (Bump (..))
import HConf.Utils.Class
  ( Format (..),
    Parse (..),
  )
import HConf.Utils.Core (Msg (..), throwError)
import HConf.Utils.FromConf (ReadConf, readFromConf)
import HConf.Utils.Source (formatList, fromToString, sepBy, toError)
import Relude hiding (show)

data Version = Version
  { major :: Int,
    minor :: Int,
    revision :: [Int]
  }
  deriving
    ( Generic,
      Eq
    )

readVersion :: (ReadConf m Version) => m Version
readVersion = readFromConf ()

getNumber :: [Int] -> Int
getNumber (n : _) = n
getNumber [] = 0

nextVersion :: Bump -> Version -> Version
nextVersion Major Version {..} = Version {major = major + 1, minor = 0, revision = [0], ..}
nextVersion Minor Version {..} = Version {minor = minor + 1, revision = [0], ..}
nextVersion Patch Version {..} = Version {revision = [getNumber revision + 1], ..}

dropPatch :: Version -> Version
dropPatch Version {..} = Version {revision = [0], ..}

compareSeries :: (Ord a) => [a] -> [a] -> Ordering
compareSeries [] _ = EQ
compareSeries _ [] = EQ
compareSeries (x : xs) (y : ys)
  | x == y = compareSeries xs ys
  | otherwise = compare x y

instance Format Version where
  format = formatList "." . toSeries

instance Parse Version where
  parse s = toError ("invalid version(" <> msg s <> ")") (sepBy "." s >>= fromSeries)

fromSeries :: (MonadFail m) => [Int] -> m Version
fromSeries [] = throwError "version should have at least one number!"
fromSeries [major] = pure Version {major, minor = 0, revision = []}
fromSeries (major : (minor : revision)) = pure Version {..}

toSeries :: Version -> [Int]
toSeries Version {..} = [major, minor] <> revision

instance ToString Version where
  toString = toString . format

instance Ord Version where
  compare a b = compareSeries (toSeries a) (toSeries b)

instance Show Version where
  show = toString

instance ToText Version where
  toText = fromToString

instance FromJSON Version where
  parseJSON (String s) = parse s
  parseJSON (Number n) = parse (fromToString $ show n)
  parseJSON v = throwError $ "version should be either true or string" <> msg v

instance ToJSON Version where
  toJSON = String . toText
