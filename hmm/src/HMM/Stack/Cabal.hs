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
    getField,
    throwError,
    withThrow,
  )
import HMM.Utils.Execute (execute, parseWarnings, printWarnings)
import HMM.Utils.Log
  ( alert,
    field,
    task,
  )
import HMM.Utils.Source
  ( fromByteString,
    ignoreEmpty,
    indentText,
    parseField,
    parseLines,
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

getCabal :: (HIO m) => FilePath -> m Cabal
getCabal path = withThrow (read path) >>= parse . fromByteString

stack :: (HIO m) => String -> PkgDir -> [String] -> m ()
stack cmd pkg options = do
  result <- execute "stack" [cmd, toString pkg] options
  case result of
    Left out -> alert $ cmd <> ": " <> unpack (indentText $ pack out)
    Right out -> printWarnings cmd (parseWarnings out)

upload :: (HIO m) => PkgName -> m ()
upload pkg = do
  result <- execute "stack" ["upload", toString pkg] []
  case result of
    Left out -> fail $ "upload: " <> unpack (indentText $ pack out)
    Right out -> printWarnings "upload" (parseWarnings out)

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
