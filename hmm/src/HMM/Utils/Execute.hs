{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Utils.Execute
  ( execute,
    isSuccess,
  )
where

import GHC.IO.Exception (ExitCode (..))
import Relude
  ( Applicative (pure),
    Bool (..),
    Either (..),
    FilePath,
    MonadIO (..),
    Semigroup ((<>)),
    String,
    map,
    ($),
  )
import System.Process (readProcessWithExitCode)

type Result = Either String

execute :: (MonadIO m) => FilePath -> [String] -> [String] -> m (Result String)
execute name args options = do
  (code, _, out) <- liftIO (readProcessWithExitCode name (args <> map ("--" <>) options) "")
  pure
    $ if isSuccess code
      then Right out
      else Left out

isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess = True
isSuccess ExitFailure {} = False
