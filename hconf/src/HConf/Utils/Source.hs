{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Source
  ( parseField,
    parseLines,
    ignoreEmpty,
    fromByteString,
    firstWord,
    removeHead,
    unconsM,
    sepBy,
    toError,
    fromToString,
    isIndentedLine,
    indentText,
    startsLike,
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.Char (isSeparator)
import Data.Text
  ( break,
    concatMap,
    head,
    isPrefixOf,
    null,
    pack,
    singleton,
    split,
    splitOn,
    strip,
    toLower,
    uncons,
    drop
  )
import qualified Data.Text as T
import HConf.Utils.Class (Parse (..))
import HConf.Utils.Core
  ( ErrorMsg,
    maybeToError,
    throwError,
  )
import Relude hiding
  ( break,
    concatMap,
    drop,
    head,
    isPrefixOf,
    null,
    uncons,
  )

-- terms

startsLike :: Text -> Text -> Bool
startsLike x y = toLower x `isPrefixOf` toLower y

replaceNewLine :: Char -> Text
replaceNewLine '\n' = "          \n"
replaceNewLine x = singleton x

indentText :: Text -> Text
indentText = concatMap replaceNewLine

parseField :: Text -> (Text, Text)
parseField = second (drop 1) . breakAt (== ':')

firstWord :: Text -> (Text, Text)
firstWord = breakAt isSeparator

-- lexer
parseLines :: Text -> [Text]
parseLines = split (== '\n')

ignoreEmpty :: [(Text, b)] -> [(Text, b)]
ignoreEmpty = filter (not . null . fst)

fromByteString :: BS.ByteString -> Text
fromByteString = pack . BS.unpack

isIndentedLine :: Text -> Bool
isIndentedLine line = head line == ' '

ignoreSpaces :: Text -> Text
ignoreSpaces = T.filter (not . isSeparator)

breakAt :: (Char -> Bool) -> Text -> (Text, Text)
breakAt f = bimap strip strip . break f . strip

sepBy :: (MonadFail m, Parse a) => Text -> Text -> m [a]
sepBy sep = traverse parse . splitOn sep . ignoreSpaces

removeHead :: Char -> Text -> (Bool, Text)
removeHead should txt = maybe (False, txt) has (uncons txt)
  where
    has (x, xs)
      | x == should = (True, xs)
      | otherwise = (False, txt)

unconsM :: (MonadFail m) => String -> Text -> m (Text, Text)
unconsM m x = first singleton <$> maybeToError (m <> "<>: " <> toString x) (uncons x)

toError :: (MonadFail m) => ErrorMsg -> Either ErrorMsg a -> m a
toError label (Left s) = throwError $ label <> ": " <> s
toError _ (Right a) = pure a

fromToString :: (ToString a) => a -> Text
fromToString = pack . toString
