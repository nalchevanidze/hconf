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
import Data.List (maximum, minimum)
import HConf.Core.HkgRef (fetchVersions)
import HConf.Core.Version (Version, dropPatch, nextVersion)
import HConf.Utils.Chalk (Color (Yellow), chalk)
import HConf.Utils.Class
  ( Diff (..),
    Format (..),
    HConfIO,
    Parse (..),
  )
import HConf.Utils.Core (DependencyName (..), Msg (..), throwError, withString)
import HConf.Utils.FromConf (ByKey)
import HConf.Utils.Log (field)
import HConf.Utils.Source (formatList, fromToString, removeHead, sepBy, unconsM)
import Relude

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

type BoundsByName = ByKey DependencyName Bounds

instance Parse Bounds where
  parse "" = pure $ Bounds []
  parse str = Bounds <$> sepBy "&&" str

instance Format Bounds where
  format (Bounds xs) = formatList " && " $ sort xs

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

getLatest :: (HConfIO m) => DependencyName -> m Bound
getLatest = fmap (Bound Max True . head) . fetchVersions

updateDepBounds :: (HConfIO m) => DependencyName -> Bounds -> m Bounds
updateDepBounds name bounds = do
  latest <- getLatest name
  let upper = getBound Max bounds
  let newVersion = maximum (latest : upper)
  if upper == [newVersion] then pure () else field name (show newVersion)
  _min <- initiateMin name bounds
  pure (Bounds (_min <> [newVersion]))

initiateMin :: (HConfIO f) => DependencyName -> Bounds -> f [Bound]
initiateMin name bounds = do
  let mi = getBound Min bounds
  if null mi
    then do
      ls <- fmap (Bound Min True) <$> fetchVersions name
      pure [minimum ls]
    else pure mi
