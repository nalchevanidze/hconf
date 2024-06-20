{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Stack.Cabal
  ( checkCabal,
  )
where

import Data.Map (lookup)
import Data.Text (pack, unpack)
import GHC.IO.Exception (ExitCode (..))
import HConf.Core.Version (Version)
import HConf.Utils.Class (HConfIO (..), Parse (..))
import HConf.Utils.Core (Name, maybeToError)
import HConf.Utils.Log (Log, alert, field, subTask, task, warn)
import HConf.Utils.Source (fromByteString, ignoreEmpty, indentText, isIndentedLine, parseField, parseLines, startsLike)
import HConf.Utils.Yaml (removeIfExists)
import Relude hiding (isPrefixOf)
import System.Process
import HConf.Core.PkgDir (PkgDir)

type Con m = (HConfIO m, Log m)

parseFields :: ByteString -> Map Text Text
parseFields =
  fromList
    . ignoreEmpty
    . map parseField
    . parseLines
    . fromByteString

getField :: (MonadFail m) => Name -> Map Name a -> m a
getField k = maybeToError ("missing field" <> toString k) . lookup k

cabalPath :: String -> Text -> String
cabalPath path pkgName = path <> "/" <> unpack pkgName <> ".cabal"

getCabalFields :: (Con m) => FilePath -> Name -> m (Name, Version)
getCabalFields path pkgName = do
  bs <- read (cabalPath path pkgName)
  let fields = parseFields bs
  name <- getField "name" fields
  version <- getField "version" fields >>= parse
  field name (show version)
  pure (name, version)

stack :: (Con m) => String -> String -> [String] -> m ()
stack l name options = do
  (code, _, out) <- liftIO (readProcessWithExitCode "stack" (l : (name : map ("--" <>) options)) "")
  case code of
    ExitFailure {} -> alert $ l <> ": " <> unpack (indentText $ pack out)
    ExitSuccess {} -> printWarnings (pack l) (parseWarnings out)

printWarnings :: (Con m) => Name -> [(Text, [Text])] -> m ()
printWarnings name [] = field name "ok"
printWarnings name xs = task name $ traverse_ subWarn xs
  where
    subWarn (x, ls) =
      warn (unpack x)
        >> traverse_ (warn . unpack) ls

parseWarnings :: String -> [(Text, [Text])]
parseWarnings = mapMaybe toWarning . groupTopics . parseLines . pack

groupTopics :: [Text] -> [[Text]]
groupTopics = regroup . break emptyLine
  where
    emptyLine = (== "")
    regroup (h, t)
      | null t = [h]
      | otherwise = h : groupTopics (dropWhile emptyLine t)

toWarning :: [Text] -> Maybe (Text, [Text])
toWarning (h : lns) | startsLike "warning" h = Just (h, takeWhile isIndentedLine lns)
toWarning _ = Nothing

buildCabal :: (Con m) => String -> m ()
buildCabal name = do
  stack "build" name ["test", "dry-run"]
  stack "sdist" name []

checkCabal :: (Con m) => PkgDir -> Name -> Version -> m ()
checkCabal path name version = subTask "cabal" $ do
  liftIO (removeIfExists (cabalPath (unpack path) name))
  buildCabal (toString path)
  (pkgName, pkgVersion) <- getCabalFields (unpack path) name
  if pkgVersion == version && pkgName == name
    then pure ()
    else fail (toString path <> "mismatching version or name")
