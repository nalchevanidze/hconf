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
  parse bs =
    Cabal
      <$> getField "name" fields
      <*> (getField "version" fields >>= parse)
    where
      fields =
        fromList
          $ ignoreEmpty
          $ map parseField
          $ parseLines bs

data Warning = Warning Text [Text]

getCabal :: (Con m) => FilePath -> m Cabal
getCabal path = withThrow (read path) >>= parse . fromByteString

stack :: (Con m) => String -> PkgDir -> [String] -> m ()
stack cmd pkg options = do
  (code, _, out) <- liftIO (readProcessWithExitCode "stack" (cmd : (unpack (toText pkg) : map ("--" <>) options)) "")
  case code of
    ExitFailure {} -> alert $ cmd <> ": " <> unpack (indentText $ pack out)
    ExitSuccess {} -> printWarnings (pack cmd) (parseWarnings out)

printWarnings :: (Con m) => Name -> [Warning] -> m ()
printWarnings cmd [] = field cmd "ok"
printWarnings cmd xs = task cmd $ traverse_ subWarn xs
  where
    subWarn (Warning x ls) =
      warn (unpack x)
        >> traverse_ (warn . unpack) ls

parseWarnings :: String -> [Warning]
parseWarnings = mapMaybe toWarning . groupTopics . parseLines . pack

toWarning :: [Text] -> Maybe Warning
toWarning (h : lns) | startsLike "warning" h = Just $ Warning h $ takeWhile isIndentedLine lns
toWarning _ = Nothing

groupTopics :: [Text] -> [[Text]]
groupTopics = regroup . break emptyLine
  where
    emptyLine = (== "")
    regroup (h, t)
      | null t = [h]
      | otherwise = h : groupTopics (dropWhile emptyLine t)

checkCabal :: (Con m) => PkgDir -> Cabal -> m ()
checkCabal pkg target@Cabal {..} = subTask "cabal" $ do
  let path = cabalFile name pkg
  removeIfExists path
  stack "build" pkg ["test", "dry-run"]
  stack "sdist" pkg []
  cabal <- getCabal path
  field name (show version)
  unless (cabal == target) (throwError $ "mismatching version or name" <> msg pkg)
