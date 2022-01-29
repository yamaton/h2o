{-# LANGUAGE OverloadedStrings #-}

module ExtractDescription where


import Text.ParserCombinators.ReadP
import qualified Data.Text as T
import qualified Data.List as List

import qualified Utils
import Layout (chunksByHeaders)
import Utils (infoMsg)


parseDescription :: [String] -> String -> String
parseDescription cmdSeq content = desc
  where
    xs = lines content
    chunks = chunksByHeaders xs
    candidates = filter (\lines -> "DESCRIPTION" `List.isPrefixOf` head lines) chunks
    desc = if null candidates || null (tail $ head candidates)
              then infoMsg ("Failed: Description: " ++ unwords cmdSeq) $ List.intercalate "-" cmdSeq
              else (head . tail . head) candidates
