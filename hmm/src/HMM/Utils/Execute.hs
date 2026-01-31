{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Utils.Execute
  ( execute,
    isSuccess,
    Warning (..),
    printWarnings,
    parseWarnings,
  )
where

import Data.Text (pack, unpack)
import GHC.IO.Exception (ExitCode (..))
import HMM.Utils.Class (HIO, Log (..))
import HMM.Utils.Log
  ( field,
    task,
    warn,
  )
import HMM.Utils.Source
  ( isIndentedLine,
    parseLines,
    startsLike,
  )
import Relude
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

data Warning = Warning Text [Text]

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
