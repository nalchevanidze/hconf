{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Log
  ( label,
    task,
    warn,
    alert,
    logFileChange,
    Log (..),
    info,
    field,
    subTask,
    FLog,
  )
where

import HConf.Utils.Chalk (Color (..), chalk)
import HConf.Utils.Core (Name)
import Relude

class Log m where
  log :: String -> m ()
  inside :: m a -> m a

instance Log IO where
  log = putStrLn
  inside = id

newLine :: (Log m) => m ()
newLine = log ""

li :: (ToString a) => a -> String
li e = "- " <> toString e <> ":"

label :: (Log m, Monad m) => String -> m () -> m ()
label name m = info (li name) >> newLine >> inside m >> newLine

task :: (Log m, Monad m) => Name -> m a -> m a
task name m = log (chalk Magenta (li name)) >> inside m

subTask :: (Log m, Monad m) => Name -> m a -> m a
subTask name m = log (chalk Cyan (li name)) >> inside m

field :: (Log m) => Name -> String -> m ()
field name = log . ((toString name <> ": ") <>)

logFileChange :: (Log m) => String -> Bool -> m ()
logFileChange path noChange
  | noChange = field "checked" $ chalk Gray path
  | otherwise = field "updated" $ chalk Yellow path

info :: (Log m) => String -> m ()
info = log . chalk Green

warn :: (Log m) => String -> m ()
warn = log . chalk Yellow

alert :: (Log m) => String -> m ()
alert = log . chalk Red

class FLog a where
  flog :: (Log m) => a -> m a
