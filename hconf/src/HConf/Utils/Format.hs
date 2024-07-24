{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Format
  ( formatTable,
  )
where

import Data.List (maximum)
import Data.Text (intercalate, justifyLeft, length, strip, words)
import Relude hiding (intercalate, length, words)

type Table = [Row]

type Row = [Text]

getSizes :: Table -> [Int]
getSizes xs = map size (transpose xs)
  where
    size :: Row -> Int
    size = maximum . map length

printRow :: [Int] -> Row -> Text
printRow sizes ls =
  strip
    $ intercalate "  "
    $ zipWith (\item s -> justifyLeft s ' ' item) ls sizes

formatTable :: [Text] -> [Text]
formatTable deps = sort $ map (printRow (getSizes table)) table
  where
    table = map words deps
