{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Version
  ( nextVersion,
    dropPatch,
    checkVersion,
    Version,
    fetchVersions,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.List.NonEmpty (toList)
import Data.Map (lookup)
import GHC.Show (Show (..))
import HConf.Utils.Class (Parse (..))
import HConf.Utils.Core (Name, checkElem, maybeToError, throwError, Msg (..))
import HConf.Utils.Http (hackage)
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

instance Parse Version where
  parse s = toError "invalid version" (sepBy "." s >>= fromSeries)

fromSeries :: (MonadFail m) => [Int] -> m Version
fromSeries [] = throwError "version should have at least one number!"
fromSeries [major] = pure Version {major, minor = 0, revision = []}
fromSeries (major : (minor : revision)) = pure Version {..}

toSeries :: Version -> [Int]
toSeries Version {..} = [major, minor] <> revision

instance ToString Version where
  toString = intercalate "." . map show . toSeries

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

fetchVersionResponse :: (MonadIO m, MonadFail m) => Name -> m (Either String (Map Text (NonEmpty Version)))
fetchVersionResponse name = hackage ["package", name, "preferred"]

lookupVersions :: (MonadFail m) => Either String (Map Text (NonEmpty Version)) -> m (NonEmpty Version)
lookupVersions (Right x) = maybeToError ("field normal-version not found" :: String) (lookup "normal-version" x)
lookupVersions (Left x) = throwError x

fetchVersions :: (MonadFail m, MonadIO m) => Name -> m (NonEmpty Version)
fetchVersions name = fetchVersionResponse name >>= lookupVersions

checkVersion :: (MonadFail m, MonadIO m) => (Name, Version) -> m ()
checkVersion (name, version) = fetchVersions name >>= checkElem "version" name version . toList
