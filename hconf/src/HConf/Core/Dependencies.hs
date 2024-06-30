{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Dependencies
  ( Dependencies,
    Dependency,
    getBounds,
    traverseDeps,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
  )
import Data.Map (fromList, toList)
import Data.Map.Strict (traverseWithKey)
import HConf.Core.Bounds (Bounds)
import HConf.Utils.Class (Format (format), Parse (..))
import HConf.Utils.Core (Name, select)
import HConf.Utils.Format (formatTable)
import HConf.Utils.Source (firstWord)
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

data Dependency = Dependency Name Bounds

instance Parse Dependency where
  parse =
    (\(name, txt) -> Dependency name <$> parse txt)
      . firstWord

instance Format Dependency where
  format (Dependency name b) = name <> " " <> format b

newtype Dependencies = Dependencies {unpackDeps :: Map Name Bounds}
  deriving (Show)

getBounds :: (MonadFail m) => Name -> Dependencies -> m Bounds
getBounds name = select "Package " name . unpackDeps

traverseDeps :: (Applicative f) => (Name -> Bounds -> f Bounds) -> Dependencies -> f Dependencies
traverseDeps f (Dependencies xs) = Dependencies <$> traverseWithKey f xs

initDependencies :: [Dependency] -> Dependencies
initDependencies = Dependencies . fromList . map toDuple
  where
    toDuple (Dependency a b) = (a, b)

toDependencyList :: Dependencies -> [Dependency]
toDependencyList (Dependencies m) = map (uncurry Dependency) $ toList m

instance FromJSON Dependencies where
  parseJSON v = initDependencies <$> (parseJSON v >>= traverse parse . sort)

instance ToJSON Dependencies where
  toJSON = toJSON . formatTable . map format . toDependencyList
