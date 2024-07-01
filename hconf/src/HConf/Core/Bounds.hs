{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Bounds
  ( Bounds,
    versionBounds,
    diff,
    updateUpperBound,
    ReadBounds (..),
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
import HConf.Utils.Class (Format (..), Parse (..))
import HConf.Utils.Core (Msg (..), Name, throwError, withString)
import HConf.Utils.Log (Log, field)
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
  format Bound {..} = unwords $ (toText restriction <> if orEquals then "=" else "") : [toText version]

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

diff :: Bounds -> Bounds -> String
diff old deps = toString old <> chalk Yellow "  ->  " <> toString deps

getBound :: Restriction -> Bounds -> Maybe Bound
getBound v (Bounds xs) = find (\Bound {..} -> restriction == v) xs

getLatestBound :: (MonadFail m, MonadIO m) => Name -> m Bound
getLatestBound = fmap (Bound Max True . head) . fetchVersions

updateUpperBound :: (MonadFail m, MonadIO m, Log m) => Name -> Bounds -> m Bounds
updateUpperBound name bounds = do
  latest <- getLatestBound name
  let ma = getBound Max bounds
  let mi = maybeToList (getBound Min bounds)
  let newVersion = maximum (latest : maybeToList ma)
  if ma == Just newVersion then pure () else field name (show newVersion)
  pure (Bounds (mi <> [newVersion]))

class (MonadFail m, MonadIO m, Log m) => ReadBounds m where
  readBounds :: Name -> m Bounds
