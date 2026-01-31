{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Core.Dependencies
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
import HMM.Core.Bounds (Bounds)
import HMM.Utils.Class (Format (format), Parse (..))
import HMM.Utils.Core (DependencyName, select)
import HMM.Utils.Source (firstWord, formatTable)
import Relude hiding
  ( Undefined,
    break,
    drop,
    fromList,
    length,
    null,
    show,
    toList,
  )

data Dependency = Dependency
  { name :: DependencyName,
    bounds :: Bounds
  }

instance Parse Dependency where
  parse =
    (\(name, txt) -> Dependency <$> parse name <*> parse txt)
      . firstWord

instance Format Dependency where
  format Dependency {..} = format name <> " " <> format bounds

newtype Dependencies = Dependencies {unpackDeps :: Map DependencyName Bounds}
  deriving (Show)

getBounds :: (MonadFail m) => DependencyName -> Dependencies -> m Bounds
getBounds name = select "Package " name . unpackDeps

traverseDeps :: (Applicative f) => (DependencyName -> Bounds -> f Bounds) -> Dependencies -> f Dependencies
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
