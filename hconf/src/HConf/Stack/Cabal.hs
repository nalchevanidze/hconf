{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Stack.Cabal
  ( checkCabal,
    Cabal (..),
  )
where

import Data.Text (pack, unpack)
import GHC.IO.Exception (ExitCode (..))
import HConf.Core.PkgDir (PkgDir, cabalFile)
import HConf.Core.Version (Version)
import HConf.Utils.Class (HConfIO (..), Parse (..), withThrow)
import HConf.Utils.Core (Msg (..), Name, select, throwError)
import HConf.Utils.Log (Log, alert, field, subTask, task, warn)
import HConf.Utils.Source (fromByteString, ignoreEmpty, indentText, isIndentedLine, parseField, parseLines, startsLike)
import HConf.Utils.Yaml (removeIfExists)
import Relude hiding (isPrefixOf)
import System.Process

type Con m = (HConfIO m, Log m)

data Cabal = Cabal
  { name :: Name,
    version :: Version
  }
  deriving (Eq)

getField :: (MonadFail m) => Name -> Map Name a -> m a
getField = select "Field"

instance Parse Cabal where
  parse bs = do
    name <- getField "name" fields
    version <- getField "version" fields >>= parse
    pure $ Cabal {..}
    where
      fields =
        fromList
          $ ignoreEmpty
          $ map parseField
          $ parseLines bs

getCabal :: (Con m) => PkgDir -> Name -> m Cabal
getCabal dir name = withThrow (read $ cabalFile name dir) >>= parse . fromByteString

stack :: (Con m) => String -> PkgDir -> [String] -> m ()
stack l name options = do
  (code, _, out) <- liftIO (readProcessWithExitCode "stack" (l : (unpack (toText name) : map ("--" <>) options)) "")
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

buildCabal :: (Con m) => PkgDir -> m ()
buildCabal name = do
  stack "build" name ["test", "dry-run"]
  stack "sdist" name []

checkCabal :: (Con m) => PkgDir -> Cabal -> m ()
checkCabal pkg target = subTask "cabal" $ do
  liftIO (removeIfExists (cabalFile (name target) pkg))
  buildCabal pkg
  cabal <- getCabal pkg (name target)
  field (name cabal) (show (version cabal))
  unless (cabal == target) (throwError $ "mismatching version or name" <> msg pkg)
