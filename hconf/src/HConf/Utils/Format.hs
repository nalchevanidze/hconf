{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Format (formatTable) where

import Data.List (maximum)
import Data.Text (intercalate, justifyLeft, length, strip, words)
import Relude hiding (intercalate, length, words)

type Table = [[Text]]

getSizes :: Table -> [Int]
getSizes xs = map size (transpose xs)
  where
    size :: [Text] -> Int
    size = maximum . map length

printRow :: [Int] -> [Text] -> Text
printRow sizes ls =
  strip
    $ intercalate "  "
    $ zipWith (\item s -> justifyLeft s ' ' item) ls sizes

formatTable :: [Text] -> [Text]
formatTable deps = sort $ map (printRow (getSizes table)) table
  where
    table = map words deps
