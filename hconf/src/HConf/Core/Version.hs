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
import qualified Data.Text as T
import GHC.Show (Show (..))
import HConf.Utils.Class
  ( Format (..),
    Parse (..),
    ReadConf,
    readFromConf,
  )
import HConf.Utils.Core (Msg (..), throwError)
import HConf.Utils.Source (fromToString, sepBy, toError)
import Relude hiding
  ( Undefined,
    break,
    drop,
    fromList,
    isPrefixOf,
    length,
    null,
    show,
    toList,
  )

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

nextVersion :: Bool -> Version -> Version
nextVersion isBreaking Version {..}
  | isBreaking = Version {minor = minor + 1, revision = [0], ..}
  | otherwise = Version {revision = [getNumber revision + 1], ..}

dropPatch :: Version -> Version
dropPatch Version {..} = Version {revision = [0], ..}

compareSeries :: (Ord a) => [a] -> [a] -> Ordering
compareSeries [] _ = EQ
compareSeries _ [] = EQ
compareSeries (x : xs) (y : ys)
  | x == y = compareSeries xs ys
  | otherwise = compare x y

instance Format Version where
  format = T.intercalate "." . map (fromString . show) . toSeries

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
