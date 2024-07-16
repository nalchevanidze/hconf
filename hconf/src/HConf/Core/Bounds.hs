{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Bounds
  ( Bounds,
    BoundsByName,
    versionBounds,
    updateDepBounds,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.List (maximum)
import Data.Text (intercalate)
import GHC.Show (Show (show))
import HConf.Core.HkgRef (fetchVersions)
import HConf.Core.Version (Version, dropPatch, nextVersion)
import HConf.Utils.Chalk (Color (Yellow), chalk)
import HConf.Utils.Class (ByName, Diff (..), Format (..), HConfIO, Parse (..))
import HConf.Utils.Core (Msg (..), Name, throwError, withString)
import HConf.Utils.Log (field)
import HConf.Utils.Source (fromToString, removeHead, sepBy, unconsM)
import Relude hiding
  ( Undefined,
    break,
    drop,
    fromList,
    intercalate,
    isPrefixOf,
    length,
    null,
    show,
    toList,
  )

data Restriction = Min | Max deriving (Show, Eq, Ord)

instance Parse Restriction where
  parse ">" = pure Min -- > 0.7.0
  parse "<" = pure Max -- <  1.0.0
  parse x = throwError $ "unsorted bound type" <> msg x

instance ToString Restriction where
  toString Min = ">" -- >  0.7.0
  toString Max = "<" -- <  1.0.0

instance ToText Restriction where
  toText = fromToString

data Bound = Bound
  { restriction :: Restriction,
    orEquals :: Bool,
    version :: Version
  }
  deriving (Show, Eq)

instance Format Bound where
  format Bound {..} = unwords $ (toText restriction <> eq) : [toText version]
    where
      eq = if orEquals then "=" else ""

instance Ord Bound where
  compare a b =
    compare (version a) (version b)
      <> compare (restriction a) (restriction b)
      <> compare (orEquals a) (orEquals b)

instance Parse Bound where
  parse txt = do
    (ch, str) <- unconsM "unsorted bound type" txt
    let (orEquals, value) = removeHead '=' str
    restriction <- parse ch
    version <- parse value
    pure Bound {..}

newtype Bounds = Bounds [Bound]
  deriving (Generic, Show, Eq)

type BoundsByName = ByName Bounds

instance Parse Bounds where
  parse "" = pure $ Bounds []
  parse str = Bounds <$> sepBy "&&" str

instance Format Bounds where
  format (Bounds xs) = intercalate " && " $ map format $ sort xs

instance ToString Bounds where
  toString = toString . format

instance FromJSON Bounds where
  parseJSON = withString "Bounds" parse

instance ToJSON Bounds where
  toJSON = String . fromToString

versionBounds :: Version -> Bounds
versionBounds version =
  Bounds
    [ Bound Min True (dropPatch version),
      Bound Max False (nextVersion True version)
    ]

instance Diff Bounds where
  diff old deps
    | old /= deps =
        Just (toString old <> chalk Yellow "  ->  " <> toString deps)
    | otherwise = Nothing

getBound :: Restriction -> Bounds -> [Bound]
getBound v (Bounds xs) = maybeToList $ find (\Bound {..} -> restriction == v) xs

getLatestBound :: (HConfIO m) => Name -> m Bound
getLatestBound = fmap (Bound Max True . head) . fetchVersions

updateDepBounds :: (HConfIO m) => Name -> Bounds -> m Bounds
updateDepBounds name bounds = do
  latest <- getLatestBound name
  let upper = getBound Max bounds
  let newVersion = maximum (latest : upper)
  if upper == [newVersion] then pure () else field name (show newVersion)
  pure (Bounds (getBound Min bounds <> [newVersion]))
