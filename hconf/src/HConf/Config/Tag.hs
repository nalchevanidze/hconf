{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.Tag
  ( Tag (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import GHC.Show (Show (show))
import HConf.Core.Version (Version)
import HConf.Utils.Class (Parse (..))
import HConf.Utils.Source (fromToString)
import Relude hiding (show)

data Tag
  = Version Version
  | Latest
  deriving
    ( Generic,
      Eq
    )

instance Parse Tag where
  parse "latest" = pure Latest
  parse s = Version <$> parse s

instance ToString Tag where
  toString Latest = "latest"
  toString (Version v) = toString v

instance Show Tag where
  show = toString

instance ToText Tag where
  toText = fromToString

instance FromJSON Tag where
  parseJSON (String s) = parse s
  parseJSON v = Version <$> parseJSON v

instance ToJSON Tag where
  toJSON = String . toText

instance Ord Tag where
  compare Latest Latest = EQ
  compare Latest Version {} = GT
  compare Version {} Latest = LT
  compare (Version v1) (Version v2) = compare v1 v2
