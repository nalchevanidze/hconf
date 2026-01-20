{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Config.Bump
  ( Bump (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import GHC.Show (Show (..))
import HMM.Utils.Class (Parse (..))
import HMM.Utils.Source (fromToString)
import Relude hiding (show)

data Bump
  = Major
  | Minor
  | Patch
  deriving
    ( Generic,
      Eq
    )

instance Parse Bump where
  parse "major" = pure Major
  parse "minor" = pure Minor
  parse "patch" = pure Patch
  parse v = fail $ "Invalid bump type: " <> toString (fromToString v)

instance ToString Bump where
  toString Major = "major"
  toString Minor = "minor"
  toString Patch = "patch"

instance Show Bump where
  show = toString

instance ToText Bump where
  toText = fromToString

instance FromJSON Bump where
  parseJSON (String s) = parse s
  parseJSON v = fail $ "Invalid bump type: " <> show v

instance ToJSON Bump where
  toJSON = String . toText
