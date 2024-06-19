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
import qualified Data.Map as M
import Data.Map.Strict (traverseWithKey)
import Data.Text (unpack)
import HConf.Core.Bounds (Bounds, printBoundParts)
import HConf.Utils.Class (Parse (..))
import HConf.Utils.Core (Name)
import HConf.Utils.Format (formatTable)
import HConf.Utils.Source (breakOnSpace)
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
      . breakOnSpace

newtype Dependencies = Dependencies {unpackDeps :: Map Name Bounds}
  deriving (Show)

getBounds :: (MonadFail m) => Name -> Dependencies -> m Bounds
getBounds name = maybe (fail $ "Unknown package: " <> unpack name) pure . M.lookup name . unpackDeps

traverseDeps :: (Applicative f) => (Name -> Bounds -> f Bounds) -> Dependencies -> f Dependencies
traverseDeps f (Dependencies xs) = Dependencies <$> traverseWithKey f xs

initDependencies :: [Dependency] -> Dependencies
initDependencies = Dependencies . fromList . map toDuple
  where
    toDuple (Dependency a b) = (a, b)

instance FromJSON Dependencies where
  parseJSON v = initDependencies <$> (parseJSON v >>= traverse parse . sort)

instance ToJSON Dependencies where
  toJSON (Dependencies m) = toJSON $ formatTable $ map (\(name, b) -> name : printBoundParts b) (toList m)
