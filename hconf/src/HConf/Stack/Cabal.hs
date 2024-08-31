{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Stack.Cabal
  ( Cabal (..),
    CabalSrc (..),
  )
where

import Data.Text (pack, unpack)
import HConf.Core.PkgDir (PkgDir, cabalFile)
import HConf.Core.Version (Version)
import HConf.Utils.Class (Check (..), HConfIO (..), Log (..), Parse (..), format)
import HConf.Utils.Core
  ( Msg (..),
    Name,
    PkgName,
    exec,
    getField,
    throwError,
    withThrow,
  )
import HConf.Utils.Log
  ( alert,
    field,
    task,
    warn,
  )
import HConf.Utils.Source
  ( fromByteString,
    ignoreEmpty,
    indentText,
    isIndentedLine,
    parseField,
    parseLines,
    startsLike,
  )
import Relude

data Cabal = Cabal
  { name :: PkgName,
    version :: Version
  }
  deriving (Eq)

instance Parse Cabal where
  parse bs =
    Cabal
      <$> (getField "name" fields >>= parse)
      <*> (getField "version" fields >>= parse)
    where
      fields =
        fromList
          $ ignoreEmpty
          $ map parseField
          $ parseLines bs

data Warning = Warning Text [Text]

getCabal :: (HConfIO m) => FilePath -> m Cabal
getCabal path = withThrow (read path) >>= parse . fromByteString

stack :: (HConfIO m) => String -> PkgDir -> [String] -> m ()
stack cmd pkg options = do
  (out, success) <- exec "stack" (cmd : (toString pkg : map ("--" <>) options))
  ( if success
      then printWarnings (pack cmd) (parseWarnings out)
      else alert $ cmd <> ": " <> unpack (indentText $ pack out)
    )

instance Log Warning where
  log (Warning x ls) = warn (unpack x) >> traverse_ (warn . unpack) ls

printWarnings :: (HConfIO m) => Name -> [Warning] -> m ()
printWarnings cmd [] = field cmd "ok"
printWarnings cmd xs = task (toString cmd) $ traverse_ log xs

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

instance Log Cabal where
  log Cabal {..} = field (format name) (show version)

instance (HConfIO m) => Check m CabalSrc where
  check CabalSrc {..} = task "cabal" $ do
    let path = cabalFile (name target) pkgDir
    remove path
    stack "build" pkgDir ["test", "dry-run"]
    stack "sdist" pkgDir []
    cabal <- getCabal path
    if cabal == target
      then log cabal
      else throwError $ "mismatching version or name" <> msg pkgDir
