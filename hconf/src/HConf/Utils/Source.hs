{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Source
  ( parseField,
    parseLines,
    ignoreEmpty,
    fromByteString,
    breakOnSpace,
    removeHead,
    unconsM,
    sepBy,
    toError,
    fromToString,
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
    splitOn,
    strip,
    uncons,
  )
import qualified Data.Text as T
import HConf.Utils.Class (Parse (..))
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

ignoreSpaces :: Text -> Text
ignoreSpaces = T.filter (not . isSeparator)

sepBy :: (MonadFail m, Parse a) => Text -> Text -> m [a]
sepBy sep = traverse parse . splitOn sep . ignoreSpaces

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

toError :: (MonadFail m) => String -> Either String a -> m a
toError label (Left s) = fail $ label <> ": " <> s
toError _ (Right a) = pure a

fromToString :: (ToString a) => a -> Text
fromToString = pack . toString
