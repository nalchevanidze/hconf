{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Source
  ( parseField,
    parseLines,
    ignoreEmpty,
    fromByteString,
    breakOnSpace,
    sepByAnd,
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.Char (isSeparator)
import Data.Text
  ( break,
    breakOn,
    drop,
    null,
    pack,
    split,
    strip,
  )
import qualified Data.Text as T
import Relude hiding (break, drop, null)

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

breakOnSpace :: Text -> (Text, Text)
breakOnSpace = trimBimap . break isSeparator

sepByAnd :: Text -> [Text]
sepByAnd = T.splitOn "&&" . T.filter (not . isSeparator)