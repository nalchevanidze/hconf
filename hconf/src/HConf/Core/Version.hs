{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Version
  ( nextVersion,
    dropPatch,
    HkgRef,
    Version,
    fetchVersions,
    hackageRefs,
    printHackageRef,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.List.NonEmpty (toList)
import qualified Data.Map as M
import Data.Text (pack)
import GHC.Show (Show (..))
import HConf.Utils.Class (Check (..), Parse (..))
import HConf.Utils.Core (Msg (..), Name, checkElem, select, throwError)
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
  parse s = toError ("invalid version(" <> msg s <> ")") (sepBy "." s >>= fromSeries)

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

type Versions = NonEmpty Version

data HkgRef = HkgRef Name Version

fetchVersions :: (MonadIO m, MonadFail m) => Name -> m Versions
fetchVersions name = hackage ["package", name, "preferred"] >>= select "Field" "normal-version"

instance Check HkgRef where
  check (HkgRef name version) = fetchVersions name >>= checkElem "version" name version . toList

hackageRefs :: Map Name Version -> [HkgRef]
hackageRefs = map (uncurry HkgRef) . M.toList

printHackageRef :: HkgRef -> Text
printHackageRef (HkgRef k ver) = k <> "-" <> pack (show ver)