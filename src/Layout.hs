{-# LANGUAGE DuplicateRecordFields #-}

-- | Layout-based option parsing for CLI help text.
--
-- This module provides the main entry points for parsing CLI help text
-- using layout-based analysis. It coordinates the overall parsing pipeline:
--
--   1. **Header Splitting** - Divide text into sections by header lines
--   2. **Layout Analysis** - Detect column structure (via "Layout.ColumnAnalysis")
--   3. **Fallback Parsing** - Use parser combinators when layout fails (via "HelpParser")
--   4. **Result Extraction** - Return parsed option-description pairs
--
-- == Processing Pipeline
--
-- @
-- Input (help text)
--     |
--     v
-- splitByHeaders (divide into sections)
--     |
--     v
-- preprocessAll (per section)
--     |
--     +---> getOptionDescriptionPairsFromLayout (primary: column analysis)
--     |         |
--     |         v
--     |     on failure: HelpParser.preprocessAllFallback
--     |
--     v
-- parseBlockwise (convert to Opt records)
-- @
--
-- == Module Structure
--
-- * "Layout" (this module) - Entry points and header-based splitting
-- * "Layout.ColumnAnalysis" - Core column detection heuristics
-- * "Layout.Usage" - Usage/synopsis section parsing
module Layout
  ( -- * Main Parsing Entry Points
    parseBlockwise,
    preprocessBlockwise,
    parseMany,

    -- * Usage Parsing
    parseUsage,

    -- * Layout Analysis (re-exported from ColumnAnalysis)
    getDescriptionOffset,
    makeRangePair,
    mergeRange,
  )
where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import Data.List.Extra (dropPrefix, dropSuffix, nubOrd, trim, trimStart)
import qualified HelpParser
import Layout.ColumnAnalysis
  ( Location,
    getDescriptionOffset,
    getNonoptLocations,
    getOptionDescriptionPairsFromLayout,
    makeRangePair,
    mergeRange,
  )
import Layout.Usage (parseUsage)
import Text.Printf (printf)
import Type (Opt)
import Utils (debugMsg, trace, warnShow)
import qualified Utils

--------------------------------------------------------------------------------
-- Header-Based Splitting
--------------------------------------------------------------------------------

-- | Get line indices of headers.
--
-- [NOTE] If there is only one shallowest-indented line,
-- it will look for second-shallowest lines.
getHeadingIndices :: [String] -> [Int]
getHeadingIndices [] = []
getHeadingIndices xs
  | count >= 2 || null indentations' = [idx | (idx, indentation) <- zip [0 ..] indentations, indentation == minval]
  | otherwise = [idx | (idx, indentation) <- zip [0 ..] indentations, indentation == secondMinval]
  where
    indentations = debugMsg "Line indentations:" $ map (\x -> if null (trim x) then 80 else length . takeWhile (== ' ') $ x) xs
    minval = List.minimum indentations
    count = length $ filter (== minval) indentations
    indentations' = filter (/= minval) indentations
    secondMinval = List.minimum indentations'

-- | Split text by top-level headers.
-- Headers are recognized by the least indentations.
--
-- Returns @(content_start_line, content)@ pairs where @content@ has the
-- header line already removed. When the heading lines themselves are option
-- lines, the whole text is returned as a single block with no header to skip.
splitByHeaders :: [String] -> [(Int, String)]
splitByHeaders [] = []
splitByHeaders xs
  | any Utils.startsWithLongOption headings = [(0, unlines xs)]
  | any Utils.startsWithShortOrOldOption headings = [(0, unlines xs)]
  | otherwise = chunks
  where
    sepIndices = getHeadingIndices xs -- separator indices
    blockIndicesRaw =
      if null sepIndices || 0 `notElem` sepIndices
        then 0 : sepIndices
        else sepIndices
    headings = map (xs !!) blockIndicesRaw
    chunks =
      [ (idx + 1, unlines (drop 1 chunkLines))
      | (idx, chunkLines) <- zip blockIndicesRaw (Utils.splitsAt xs blockIndicesRaw)
      , length chunkLines > 1
      , any Utils.startsWithDash chunkLines
      ]

--------------------------------------------------------------------------------
-- Main Parsing Functions
--------------------------------------------------------------------------------

-- | Parse (option-and-argument, description) pairs from text by applying
-- preprocessAll to each header-based block.
preprocessBlockwise :: String -> [(String, String)]
preprocessBlockwise content = Utils.infoTrace decoratedMsg $ concatMap (uncurry preprocessAll) indexBlockPairs
  where
    xs = lines content
    indexBlockPairs = splitByHeaders xs
    msg = printf "Found %d header-based blocks" (length indexBlockPairs)
    decoratedMsg = "-------- " ++ msg ++ " --------"

-- | Parse `Opt`s from multi-line text. Uses 'nubOrd' (O(n log n)) rather
-- than 'List.nub' (O(n^2)) for exact-duplicate removal; near-duplicates that
-- share a name but differ in arg/description are intentionally preserved
-- here so 'Postprocess.fixDuplicateOpts' can pick the highest-scoring one.
parseBlockwise :: String -> [Opt]
parseBlockwise "" = []
parseBlockwise s = nubOrd . concat $ results
  where
    pairs = preprocessBlockwise s
    results =
      [ (\xs -> if null xs then warnShow "⚠️ Failed pair (parseBlockwise) ⚠️ \n" (optStr, descStr) xs else xs) $
          HelpParser.parseWithOptPart optStr descStr
        | (optStr, descStr) <- pairs,
          (optStr, descStr) /= ("", "")
      ]

-- | Parse (option-and-argument, description) pairs from text.
--
-- This is the main preprocessing function that:
--
--   1. Tries layout-based analysis first (getOptionDescriptionPairsFromLayout)
--   2. Falls back to parser combinators for lines that don't fit the layout
--   3. Cleans up the results (trim whitespace, remove colons)
preprocessAll :: Int -> String -> [(String, String)]
preprocessAll = preprocessMeta preprocessSecondAttempt

preprocessMeta :: (Int -> String -> [(String, String)]) -> Int -> String -> [(String, String)]
preprocessMeta fallbackFunc lineIdxBase content = filter (/= ("", "")) $ map (Bifunctor.bimap cleanOptsArgs cleanDescription) res
  where
    xs = lines content
    may = getOptionDescriptionPairsFromLayout lineIdxBase content
    res = case may of
      Just (layoutResults, droppedOptLocs) ->
        layoutResults ++ fallbackResults
        where
          descLinesExtra = map fst $ takeHangingDescForFallback lineIdxBase droppedOptLocs content
          droppedOptLines = map fst droppedOptLocs
          lineNums = List.sort $ droppedOptLines ++ descLinesExtra
          rangeForFallback = Utils.infoShowCoords "Fallback range:" lineIdxBase $ Utils.toRanges lineNums
          paragraphs = map (Utils.getParagraph xs) rangeForFallback
          indices = map fst rangeForFallback
          fallbackResults =
            (\rrr -> if null rrr then rrr else Utils.warnMsg "⚠️ opt-desc pairs from the fallback ⚠️ \n" rrr) $
              concatMap (uncurry fallbackFunc) (zip indices paragraphs)
      Nothing ->
        trace
          "\n===============================================\n\
          \[warn] ignore layout: processing with fallback \n\
          \===============================================\n"
          $ HelpParser.preprocessAllFallback content
    cleanOptsArgs = dropSuffix ":" . trim
    cleanDescription = trimStart . dropPrefix ":" . unwords . words . Utils.smartUnwords . lines

-- | Second attempt fallback for preprocessing
preprocessSecondAttempt :: Int -> String -> [(String, String)]
preprocessSecondAttempt = preprocessMeta (\_ s -> HelpParser.preprocessAllFallback s)

-- | [Deprecated] Parse options without header-based splitting of input text
parseMany :: String -> [Opt]
parseMany "" = []
parseMany s = nubOrd . concat $ results
  where
    pairs = preprocessAll 0 s
    results =
      [ (\xs -> if null xs then warnShow "⚠️ Failed pair (parseMany) ⚠️ \n" (optStr, descStr) xs else xs) $
          HelpParser.parseWithOptPart optStr descStr
        | (optStr, descStr) <- pairs,
          (optStr, descStr) /= ("", "")
      ]

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Helper to get hanging description locations for fallback processing.
-- This is a simplified version that just gets non-option locations.
takeHangingDescForFallback :: Int -> [Location] -> String -> [Location]
takeHangingDescForFallback lineIdxBase optLocs content = descLocSelected
  where
    descLocs = getNonoptLocations content
    (optLines, _) = unzip optLocs
    (descLines, _) = unzip descLocs
    cueDescLocs =
      Utils.infoShowCoords "Cue description locations:" lineIdxBase $
        [ (descLineNum, descIndentation)
          | (i, (descLineNum, descIndentation)) <- zip [0 ..] descLocs,
            (descLineNum - 1) `elem` optLines,
            optIndentation <- take 1 [c | (r, c) <- optLocs, r == (descLineNum - 1)],
            descIndentation >= optIndentation,
            descIndentation > optIndentation
              || (descLineNum - 2) `notElem` descLines
              || i == 0
              || snd (descLocs !! (i - 1)) /= optIndentation
        ]
    (cueLines, _) = unzip cueDescLocs
    descLocChunks = Utils.groupConsecutiveByFst descLocs
    descLocChunks' = filter startsWithCueLine descLocChunks
    descLocSelected = concatMap takeSameIndent descLocChunks'

    startsWithCueLine [] = False
    startsWithCueLine ((row, _) : _) = row `elem` cueLines

    takeSameIndent [] = []
    takeSameIndent chunk@((_, indentation) : _) = takeWhile ((== indentation) . snd) chunk
