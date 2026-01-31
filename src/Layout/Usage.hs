-- | Usage and synopsis section parsing.
--
-- This module handles extraction of usage/synopsis information from CLI help text.
-- It identifies "Usage:" and "SYNOPSIS" sections and extracts the command patterns.
--
-- The main entry point is 'parseUsage' which:
--
--   1. Splits text by headers (least-indented lines)
--   2. Finds blocks starting with "Usage" or "SYNOPSIS"
--   3. Extracts the content, handling both inline and multi-line formats
--
-- For example, both of these formats are supported:
--
-- @
-- Usage: grep [OPTIONS] PATTERN [FILE...]
-- @
--
-- @
-- SYNOPSIS
--     grep [OPTIONS] PATTERN [FILE...]
-- @
module Layout.Usage
  ( parseUsage,
  )
where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Data.List.Extra (trim, trimEnd)
import qualified HelpParser
import qualified Utils

-- | Parse Usage or SYNOPSIS content from help text.
--
-- Finds sections starting with "Usage" or "SYNOPSIS" and extracts the
-- command pattern information.
parseUsage :: String -> String
parseUsage content
  | null blockFiltered = Utils.debugShow "[parseUsage] NOT FOUND" blocks ""
  | foundSynopsis = Utils.debugMsg "[parseUsage] SYNOPSIS: " result
  | foundUsage = Utils.debugMsg "[parseUsage] Usage: " result
  | otherwise = Utils.debugTrace "[parseUsage] Unexpected state: no matching condition" ""
  where
    headerSynopsis = "SYNOPSIS"
    headerUsage = "Usage"
    keywords = [headerSynopsis, headerUsage]
    toLower = map Char.toLower
    isPrefixOf prefix s = toLower prefix == toLower (take (length prefix) (trimStart s))
    foundPrefix s = any (`isPrefixOf` s) keywords
    blocks = (splitByHeadersForUsage . lines) content
    blockFiltered = filter foundPrefix blocks
    theBlock = Utils.debugMsg "[parseUsage] Selected block:" $ head blockFiltered
    foundSynopsis = headerSynopsis `isPrefixOf` theBlock
    foundUsage = headerUsage `isPrefixOf` theBlock
    getBody = trimEnd . unlines . trimFixedIndents . tail . lines

    xs = lines theBlock
    firstLine = head xs
    result
      | Maybe.isNothing offsetMaybe = Utils.debugShow "[parseUsage] Something is wrong" firstLine ""
      | (null . trim) firstLineRest = Utils.debugMsg "[parseUsage] Header-only first line:" $ getBody theBlock
      | otherwise = trimEnd (unlines ys)
      where
        offsetMaybe = length <$> HelpParser.parseUsageHeader keywords theBlock
        offset = Utils.debugMsg "[parseUsage] Detected offset:" $ Maybe.fromJust offsetMaybe
        firstLineRest = drop offset firstLine
        pairs = map (splitAt offset) xs
        ys = snd (head pairs) : map snd (takeWhile (null . trim . fst) (tail pairs))

    -- Local helper to avoid importing Data.List.Extra.trimStart
    trimStart = dropWhile (== ' ')

-- | Split text by top-level headers.
-- Headers are recognized by the least indentations.
--
-- NOTE: the top-level headers are **included** in the output.
-- This does not exclude headings starting with "- Hey this is heading!"
splitByHeadersForUsage :: [String] -> [String]
splitByHeadersForUsage xs = chunks
  where
    sepIndices = getHeadingIndicesSimple xs
    blockHeadIndices =
      if null sepIndices || 0 `notElem` sepIndices
        then 0 : sepIndices
        else sepIndices
    chunks = map unlines (Utils.splitsAt xs blockHeadIndices)

-- | Get line indices of the shallowest-indented lines.
getHeadingIndicesSimple :: [String] -> [Int]
getHeadingIndicesSimple [] = []
getHeadingIndicesSimple xs =
  [idx | (idx, indentation) <- zip [0 ..] indentations, indentation == minval]
  where
    indentations =
      map (\x -> if null (trim x) then 80 else length . takeWhile (== ' ') $ x) xs
    minval = minimum indentations

-- | Drop by the shallowest indentation in the given lines.
trimFixedIndents :: [String] -> [String]
trimFixedIndents xs
  | null ys = Utils.debugMsg "[parseUsage] No non-empty lines found:" xs
  | otherwise = Utils.debugMsg "[parseUsage] Trimmed lines:" $ map (drop size) xs
  where
    ys = filter (not . null . trim) xs
    size = minimum (map (length . takeWhile (== ' ')) ys)
