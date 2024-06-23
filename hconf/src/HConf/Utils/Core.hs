{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Utils.Core
  ( compareFields,
    maybeList,
    toKebabCase,
    Name,
    tupled,
    aesonYAMLOptions,
    checkElem,
    notElemError,
    maybeToError,
    maybeMapToList,
    maybeBool,
    throwError,
    Msg (..),
    ErrorMsg (..),
    jsonString,
  )
where

import Data.Aeson (Options (..), Value (..), defaultOptions, encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (isUpper, toLower)
import Data.List (elemIndex, intercalate)
import qualified Data.Map as M
import Data.Text (toTitle)
import Relude hiding (Undefined, intercalate)
import Text.URI (URI)

aesonYAMLOptions :: Options
aesonYAMLOptions = defaultOptions {fieldLabelModifier = toKebabCase}

type Name = Text

fields :: [Text]
fields =
  map
    toTitle
    [ "name",
      "version",
      "github",
      "license",
      "author",
      "category",
      "synopsis",
      "maintainer",
      "homepage",
      "copyright",
      "license-file",
      "description",
      "bounds",
      "ghc",
      "resolver",
      "packages",
      "groups",
      "builds",
      "extra-source-files",
      "data-files",
      "main",
      "source-dirs",
      "ghc-options",
      "dependencies",
      "library",
      "executables",
      "include",
      "exclude",
      "allow-newer",
      "save-hackage-creds",
      "extra-deps",
      "stackYaml",
      "components",
      "path",
      "component"
    ]

getIndex :: Text -> Maybe Int
getIndex = (`elemIndex` fields)

type TupleRes a = (Text, Text) -> (a, a)

mapTuple :: (Text -> a) -> TupleRes a
mapTuple f = bimap f f

compareFieldNames :: (Text, Text) -> Ordering
compareFieldNames t = case mapTuple getIndex t of
  (Nothing, Nothing) -> uncurry compare t
  (Nothing, _) -> GT
  (_, Nothing) -> LT
  (i1, i2) -> compare i1 i2

compareFieldsTuple :: (Text, Text) -> Ordering
compareFieldsTuple = compareFieldNames . mapTuple toTitle

compareFields :: Text -> Text -> Ordering
compareFields = curry compareFieldsTuple

maybeList :: Maybe [a] -> [a]
maybeList = fromMaybe []

toKebabCase :: String -> String
toKebabCase = concatMap toKebab
  where
    toKebab
      x
        | isUpper x = ['-', toLower x]
        | otherwise = [x]

tupled :: (Functor f) => (t -> f a) -> t -> f (t, a)
tupled f p = (p,) <$> f p

newtype ErrorMsg = ErrorMsg String deriving (Semigroup, IsString, ToString)

class Msg a where
  msg :: a -> ErrorMsg

instance Msg Text where
  msg = ErrorMsg . toString

instance Msg String where
  msg = ErrorMsg

instance Msg Value where
  msg = ErrorMsg . unpack . encode

instance Msg URI where
  msg = ErrorMsg . show

jsonString :: (MonadFail m) => Text -> Value -> m Text
jsonString _ (String p) = pure p
jsonString label v = throwError ("cant parse" <> msg label <> "expected string got" <> msg v)

oneOfMsg :: (ToString a) => [a] -> ErrorMsg
oneOfMsg xs = ErrorMsg $ "one of:" <> intercalate ", " (map toString xs)

throwError :: (MonadFail m) => ErrorMsg -> m a2
throwError = fail . toString

maybeToError :: (MonadFail m, Msg s) => s -> Maybe a -> m a
maybeToError m = maybe (throwError (msg m)) pure

notElemError :: (MonadFail m, Eq t, ToString t) => Name -> Name -> [t] -> m a
notElemError name listName xs =
  throwError
    ( "no matching "
        <> msg name
        <> " for '"
        <> msg listName
        <> "'! try "
        <> oneOfMsg xs
    )

checkElem :: (MonadFail m, Eq t, ToString t) => Name -> Name -> t -> [t] -> m ()
checkElem name listName x xs =
  if x `elem` xs
    then pure ()
    else notElemError name listName xs

maybeMapToList :: Maybe (Map k a) -> [(k, a)]
maybeMapToList = maybe [] M.toList

maybeBool :: Maybe Bool -> Bool
maybeBool = fromMaybe False
