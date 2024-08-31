{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Log
  ( task,
    warn,
    alert,
    logFileChange,
    info,
    field,
  )
where

import HConf.Utils.Chalk (Color (..), chalk)
import HConf.Utils.Class (Format (..), HConfIO (..))
import Relude

li :: (ToString a) => Int -> a -> String
li 0 e = "\n" <> toString e <> "\n"
li _ e = "- " <> toString e <> ":"

color :: Int -> Color
color 0 = Green
color 1 = Magenta
color 2 = Cyan
color _ = Gray

task :: (HConfIO m) => String -> m a -> m a
task name = inside (\i -> chalk (color i) (li i name))

field :: (Format a, HConfIO m) => a -> String -> m ()
field name = fieldInternal (toString (format name) <> ": ")

fieldInternal :: (HConfIO m) => String -> String -> m ()
fieldInternal name = putLine . ((name <> ": ") <>)

logFileChange :: (HConfIO m) => String -> Bool -> m ()
logFileChange path noChange
  | noChange = fieldInternal "checked" $ chalk Gray path
  | otherwise = fieldInternal "updated" $ chalk Yellow path

info :: (HConfIO m) => String -> m ()
info = putLine . chalk Green

warn :: (HConfIO m) => String -> m ()
warn = putLine . chalk Yellow

alert :: (HConfIO m) => String -> m ()
alert = putLine . chalk Red
