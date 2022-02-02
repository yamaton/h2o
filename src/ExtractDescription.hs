{-# LANGUAGE OverloadedStrings #-}

module ExtractDescription where

import qualified Data.Text as T
import Layout (chunksByHeaders)
import Utils (infoMsg)
import Data.Text (Text)


getSection :: Text -> [String] -> Text -> Text
getSection prefix cmdSeq content = desc
  where
    xs = T.lines content
    chunks = map (map T.pack) (chunksByHeaders (map T.unpack xs))
    candidates = filter (\chunk -> prefix `T.isPrefixOf` head chunk) chunks
    cmd = unwords cmdSeq
    desc = if null candidates || null (tail . head $ candidates) || T.null (T.strip . head . tail . head $ candidates)
              then infoMsg ("Failed: " ++ T.unpack prefix) $ T.pack cmd
              else (T.unlines . head) candidates

getDescription :: [String] -> Text -> Text
getDescription cmdSeq content = getFirstSentence $ getSection "DESCRIPTION" cmdSeq content

unhyphenate :: [Text] -> Text
unhyphenate = T.replace "- " "" . T.unwords . T.words . T.unlines


getFirstSentence :: Text -> Text
getFirstSentence t =  T.strip first
  where
    texts = tail (T.lines t)
    paragraph = unhyphenate $ takeWhile (not . T.null . T.strip) texts
    (first, _) = T.breakOn ". " (paragraph `T.append` " ")
