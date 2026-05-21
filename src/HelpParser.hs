-- | Compatibility facade for help-text parsing.
--
-- New code should prefer the narrower modules when possible:
--
-- * "HelpOptParser" parses already separated option-spec text.
-- * "HelpLineParser" heuristically splits raw help lines into option and
--   description text.
-- * "HelpMetadata" classifies QIIME/Click-style metadata and status lines.
--
-- This module keeps the historical public surface by re-exporting the option
-- and line parsers, and it owns the old high-level fallback behavior of
-- 'parseWithOptPart'.
module HelpParser
  ( module HelpLineParser,
    module HelpOptParser,
    parseUsageHeader,
    parseWithOptPart,
    headerWithOffset,
  )
where

import Data.Char (toLower)
import HelpLineParser
import qualified HelpOptParser as OptParser
import HelpOptParser
import Text.ParserCombinators.ReadP
import Type (Opt)
import Utils (trace)

-- | Get `Opt`s from already separated option text and description text.
--
-- This preserves the historical behavior: first parse the option-spec part
-- directly, then fall back to raw-line parsing if that separated part was
-- actually still a combined help line.
parseWithOptPart :: String -> String -> [Opt]
parseWithOptPart optStr descStr
  | (not . null) res = res
  | otherwise = trace "🛑🛑🛑🛑🛑 optPart parser failed, using fallback 🛑🛑🛑🛑🛑" $ HelpLineParser.parseLine (optStr ++ "   " ++ descStr)
  where
    res = OptParser.parseOptDescPair optStr descStr

-- | Extract the header title line IF it contains one of the given keywords
-- "properly". Assumes the first line of block as a header line.
parseUsageHeader :: [String] -> String -> Maybe String
parseUsageHeader _ "" = Nothing
parseUsageHeader [] _ = Nothing
parseUsageHeader keywords block =
  case lines block of
    [] -> Nothing
    headerLineRaw : _ ->
      case readP_to_S (headerWithOffset kwds) headerLine of
        [] -> Nothing
        (pair, _) : _ -> Just pair
      where
        kwds = map (map toLower) keywords
        headerLine = map toLower headerLineRaw

headerWithOffset :: [String] -> ReadP String
headerWithOffset [] = pfail
headerWithOffset names = do
  preSpaces <- munch (== ' ')
  nameFound <- foldr1 (<++) (map string names)
  postSpaces <- munch (== ' ')
  colon <- string ":" <++ pure ""
  aftSpaces <- munch (== ' ')
  let components = [preSpaces, nameFound, postSpaces, colon, aftSpaces]
  return (concat components)
