{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Stack.Cabal
  ( Cabal (..),
    CabalSrc (..),
    stack,
    upload,
  )
where

import Data.Text (pack, unpack)
import HMM.Core.PkgDir (PkgDir, cabalFile)
import HMM.Core.Version (Version)
import HMM.Utils.Class
  ( Check (..),
    HIO (..),
    Log (..),
    Parse (..),
  )
import HMM.Utils.Core
  ( Msg (..),
    PkgName,
    exec,
    getField,
    throwError,
    withThrow,
  )
import HMM.Utils.Log
  ( alert,
    field,
    task,
    warn,
  )
import HMM.Utils.Source
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

getCabal :: (HIO m) => FilePath -> m Cabal
getCabal path = withThrow (read path) >>= parse . fromByteString

stack :: (HIO m) => String -> PkgDir -> [String] -> m ()
stack cmd pkg options = do
  (out, success) <- exec "stack" (cmd : (toString pkg : map ("--" <>) options))
  ( if success
      then printWarnings cmd (parseWarnings out)
      else alert $ cmd <> ": " <> unpack (indentText $ pack out)
    )

upload :: (HIO m) => PkgName -> [String] -> m ()
upload pkg options = do
  (out, success) <- exec "stack" ("upload" : (toString pkg : map ("--" <>) options))
  ( if success
      then printWarnings "upload" (parseWarnings out)
      else alert $ "upload" <> ": " <> unpack (indentText $ pack out)
    )

instance Log Warning where
  log (Warning x ls) = warn (unpack x) >> traverse_ (warn . unpack) ls

printWarnings :: (HIO m) => String -> [Warning] -> m ()
printWarnings cmd [] = field cmd "ok"
printWarnings cmd xs = task cmd $ traverse_ log xs

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
  log Cabal {..} = field name (show version)

instance (HIO m) => Check m CabalSrc where
  check CabalSrc {..} = task "cabal" $ do
    let path = cabalFile (name target) pkgDir
    remove path
    stack "build" pkgDir ["test", "dry-run"]
    stack "sdist" pkgDir []
    cabal <- getCabal path
    if cabal == target
      then log cabal
      else throwError $ "mismatching version or name" <> msg pkgDir
