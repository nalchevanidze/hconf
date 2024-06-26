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
  )
where

import HConf.Utils.Chalk (Color (..), chalk)
import HConf.Utils.Class (Log (..))
import HConf.Utils.Core (Name)
import Relude

newLine :: (Log m) => m ()
newLine = log ""

li :: (ToString a) => a -> String
li e = "- " <> toString e <> ":"

genColor :: (Eq a, Num a) => a -> Color
genColor 1 = Green
genColor 2 = Magenta
genColor 3 = Cyan
genColor _ = Gray

task :: Name -> m a -> m a
task name = inside f
  where
    f i = chalk (genColor i) (toString name)

label :: (Log m, Monad m) => String -> m () -> m ()
label name = task (toString name)

-- task :: (Log m, Monad m) => Name -> m a -> m a
-- task name m = log (chalk Magenta (li name)) >> inside m

subTask :: (Log m, Monad m) => Name -> m a -> m a
subTask = task

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
