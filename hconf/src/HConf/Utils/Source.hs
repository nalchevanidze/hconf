{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Source
  ( trimBimap,
    parseField,
    parseLines,
    ignoreEmpty,
    fromByteString,
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.Text
  ( breakOn,
    drop,
    null,
    pack,
    split,
    strip,
  )
import Relude hiding (drop, null)

trimBimap :: (Bifunctor f) => f Text Text -> f Text Text
trimBimap = bimap strip strip

parseField :: Text -> (Text, Text)
parseField = trimBimap . (second (drop 1) . breakOn ":") . strip

parseLines :: Text -> [Text]
parseLines = split (== '\n')

ignoreEmpty :: [(Text, b)] -> [(Text, b)]
ignoreEmpty = filter (not . null . fst)

fromByteString :: BS.ByteString -> Text
fromByteString = pack . BS.unpack