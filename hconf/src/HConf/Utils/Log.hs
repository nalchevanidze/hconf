{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Log
  ( task,
    warn,
    alert,
    logFileChange,
    Log (..),
    info,
    field,
  )
where

import HConf.Utils.Chalk (Color (..), chalk)
import HConf.Utils.Class (Log (..))
import HConf.Utils.Core (Name)
import Relude

li :: (ToString a) => Int -> a -> String
li 0 e = "\n" <> toString e <> "\n"
li _ e = "- " <> toString e <> ":"

color :: (Eq a, Num a) => a -> Color
color 0 = Green
color 1 = Magenta
color 2 = Cyan
color _ = Gray

task :: (Log m) => Name -> m a -> m a
task name = inside (\i -> chalk (color i) (li i name))

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
