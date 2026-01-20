{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Utils.Log
  ( task,
    warn,
    alert,
    logFileChange,
    info,
    field,
  )
where

import HMM.Utils.Chalk (Color (..), chalk)
import HMM.Utils.Class (Format (..), HIO (..))
import Relude

li :: (ToString a) => Int -> a -> String
li 0 e = "\n" <> toString e <> "\n"
li _ e = "- " <> toString e <> ":"

color :: Int -> Color
color 0 = Green
color 1 = Magenta
color 2 = Cyan
color _ = Gray

task :: (HIO m) => String -> m a -> m a
task name = inside (\i -> chalk (color i) (li i name))

field :: (Format a, HIO m) => a -> String -> m ()
field name = fieldInternal (toString (format name) <> ": ")

fieldInternal :: (HIO m) => String -> String -> m ()
fieldInternal name = putLine . ((name <> ": ") <>)

logFileChange :: (HIO m) => String -> Bool -> m ()
logFileChange path noChange
  | noChange = fieldInternal "checked" $ chalk Gray path
  | otherwise = fieldInternal "updated" $ chalk Yellow path

info :: (HIO m) => String -> m ()
info = putLine . chalk Green

warn :: (HIO m) => String -> m ()
warn = putLine . chalk Yellow

alert :: (HIO m) => String -> m ()
alert = putLine . chalk Red
