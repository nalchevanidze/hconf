{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Source
  ( parseField,
    parseLines,
    ignoreEmpty,
    fromByteString,
    breakOnSpace,
    sepByAnd,
    removeHead,
    unconsM,
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
    uncons,
  )
import qualified Data.Text as T
import Relude hiding
  ( break,
    drop,
    null,
    uncons,
  )

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

removeHead :: Char -> Text -> (Bool, Text)
removeHead should txt = maybe (False, txt) has (uncons txt)
  where
    has (x, xs)
      | x == should = (True, xs)
      | otherwise = (False, txt)

unconsM :: (MonadFail m) => String -> Text -> m (Char, Text)
unconsM msg x =
  maybe
    (fail (msg <> "<>: " <> toString x))
    pure
    (uncons x)
