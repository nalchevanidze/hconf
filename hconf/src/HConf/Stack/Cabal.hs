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
    CabalSrc (..),
  )
where

import Data.Text (pack, unpack)
import GHC.IO.Exception (ExitCode (..))
import HConf.Core.PkgDir (PkgDir, cabalFile)
import HConf.Core.Version (Version)
import HConf.Utils.Class (HConfIO (..), Parse (..), withThrow)
import HConf.Utils.Core (Msg (..), Name, select, throwError)
import HConf.Utils.Log (FLog (..), Log, alert, field, subTask, task, warn)
import HConf.Utils.Source (fromByteString, ignoreEmpty, indentText, isIndentedLine, parseField, parseLines, startsLike)
import HConf.Utils.Yaml (remove)
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

instance FLog Warning where
  flog (Warning x ls) = warn (unpack x) >> traverse_ (warn . unpack) ls

printWarnings :: (Con m) => Name -> [Warning] -> m ()
printWarnings cmd [] = field cmd "ok"
printWarnings cmd xs = task cmd $ traverse_ flog xs

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

data CabalSrc = CabalSrc
  { pkgDir :: PkgDir,
    target :: Cabal
  }

instance FLog Cabal where
  flog Cabal {..} = field name (show version)

checkCabal :: (Con m) => CabalSrc -> m ()
checkCabal CabalSrc {..} = subTask "cabal" $ do
  let path = cabalFile (name target) pkgDir
  remove path
  stack "build" pkgDir ["test", "dry-run"]
  stack "sdist" pkgDir []
  cabal <- getCabal path
  if cabal == target
    then flog cabal
    else throwError $ "mismatching version or name" <> msg pkgDir
