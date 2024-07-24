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
    SourceText,
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.Char (isSeparator)
import Data.Text
  ( break,
    concatMap,
    drop,
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

type SourceText = Text

startsLike :: SourceText -> SourceText -> Bool
startsLike x y = toLower x `isPrefixOf` toLower y

replaceNewLine :: Char -> SourceText
replaceNewLine '\n' = "          \n"
replaceNewLine x = singleton x

indentText :: SourceText -> SourceText
indentText = concatMap replaceNewLine

parseField :: SourceText -> (SourceText, SourceText)
parseField = second (strip . drop 1) . breakAt (== ':')

firstWord :: SourceText -> (SourceText, SourceText)
firstWord = breakAt isSeparator

-- lexer
parseLines :: SourceText -> [SourceText]
parseLines = split (== '\n')

ignoreEmpty :: [(SourceText, b)] -> [(SourceText, b)]
ignoreEmpty = filter (not . null . fst)

fromByteString :: BS.ByteString -> SourceText
fromByteString = pack . BS.unpack

isIndentedLine :: SourceText -> Bool
isIndentedLine line = head line == ' '

ignoreSpaces :: SourceText -> SourceText
ignoreSpaces = T.filter (not . isSeparator)

breakAt :: (Char -> Bool) -> SourceText -> (SourceText, SourceText)
breakAt f = bimap strip strip . break f . strip

sepBy :: (MonadFail m, Parse a) => SourceText -> SourceText -> m [a]
sepBy sep = traverse parse . splitOn sep . ignoreSpaces

removeHead :: Char -> SourceText -> (Bool, SourceText)
removeHead should txt = maybe (False, txt) has (uncons txt)
  where
    has (x, xs)
      | x == should = (True, xs)
      | otherwise = (False, txt)

unconsM :: (MonadFail m) => String -> SourceText -> m (SourceText, SourceText)
unconsM m x = first singleton <$> maybeToError (m <> "<>: " <> toString x) (uncons x)

toError :: (MonadFail m) => ErrorMsg -> Either ErrorMsg a -> m a
toError label (Left s) = throwError $ label <> ": " <> s
toError _ (Right a) = pure a

fromToString :: (ToString a) => a -> SourceText
fromToString = pack . toString
