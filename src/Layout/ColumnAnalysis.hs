{-# LANGUAGE DuplicateRecordFields #-}

-- | Column-based layout analysis for CLI help text.
--
-- This module detects the columnar structure of help text to separate
-- options from their descriptions. It uses frequency-based heuristics:
--
--   1. Find lines starting with dashes (option lines)
--   2. Detect the most common horizontal offset where options appear
--   3. Detect where descriptions start (either same line or next line)
--   4. Extract option-description pairs based on detected layout
--
-- The approach handles 2-column layouts (option | description) and
-- 3-column layouts (short | long | description) automatically.
--
-- == Key Heuristics
--
-- === The 75% Alignment Threshold
--
-- When determining if a layout analysis is valid, we require that at least
-- 75% of option lines align with the detected description offset. Real-world
-- help text has noise (wrapped lines, indented sub-options, etc.), so
-- requiring 100% alignment would fail on valid input. The 75% threshold is
-- permissive enough to handle variations while still being a strong signal.
--
-- === The 3-Space Column Separator
--
-- Most CLI tools use at least 3 spaces to separate options from descriptions.
-- Single/double spaces commonly appear within option arguments (e.g., @-o FILE@).
-- Using 3+ spaces as the column boundary is a reliable heuristic.
--
-- === Offset Disagreement Resolution
--
-- Two independent methods estimate description offset:
--
--   1. From description-only lines (lines without options)
--   2. From option lines (where description follows option on same line)
--
-- When they disagree, fallback rules apply:
--
--   * If counts are low (<=3), trust the method with more evidence
--   * If offsets differ by <5 columns, trust option-line method
--     (description-only lines may have extra indent)
--   * Otherwise, return Nothing (can't determine layout)
--
-- === The "Hanging Description" Heuristic
--
-- A description "hangs" from an option if it:
--
--   * Appears on a line immediately after an option line
--   * Has indentation >= the option's indentation
--   * Is part of a contiguous block of same-indented lines
--
-- This handles multi-line descriptions that wrap below their option.
module Layout.ColumnAnalysis
  ( -- * Main Analysis Entry Points
    getOptionDescriptionPairsFromLayout,
    getDescriptionOffset,
    getDescOffsetOptLocsPair,

    -- * Range Operations
    makeRangePair,
    mergeRange,

    -- * Location Detection
    getNonoptLocations,

    -- * Types
    Location,
    Range,
  )
where

import Control.Exception (assert)
import Data.Char (isAlphaNum)
import Data.List (isInfixOf)
import qualified Data.List as List
import Data.List.Extra (breakOnEnd, nubSort, trim, trimEnd)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified HelpParser
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Printf (printf)
import qualified Utils
import Utils (debugMsg, getMostFrequent, getMostFrequentWithCount, infoMsg)

-- | Location is defined by (row, col) order
type Location = (Int, Int)

-- | Range is also defined by [startIndex, lastIndex) where half-close, half-open
type Range = (Int, Int)

-- [TODO] memoise the calls
-- https://stackoverflow.com/questions/3208258/memoization-in-haskell

--------------------------------------------------------------------------------
-- Location Detection
--------------------------------------------------------------------------------

-- | Get location right before '-' in the head of a line
--
-- >>> getOptionLocations " \n\n          --option here\n  --baba"
-- [(2, 10), (3, 2)]
getOptionLocations :: String -> [Location]
getOptionLocations = _getNonblankLocationTemplate Utils.startsWithDash

-- | Get locations of lines NOT starting with dash
--
-- >>> getNonoptLocations " \n\n          --option here\n  --baba"
-- [(0, 1), (1, 0)]
getNonoptLocations :: String -> [Location]
getNonoptLocations = _getNonblankLocationTemplate (not . Utils.startsWithDash)

-- | A helper function for getting locations of lines matching a predicate
_getNonblankLocationTemplate :: (String -> Bool) -> String -> [Location]
_getNonblankLocationTemplate f s =
  [(i, getHorizOffset x) | (i, x) <- enumLines, (not . null . trim) x, f x]
  where
    enumLines = zip [(0 :: Int) ..] (lines s)
    getHorizOffset = length . takeWhile (== ' ')

-- | Get presumed horizontal offsets of options lines.
-- Here the number is plural as the short options and the long options
-- can appear with different justifications (i.e. docker --help)
getOptionOffsets :: String -> [Int]
getOptionOffsets s = case (short, long) of
  (Nothing, Nothing) -> []
  (Nothing, Just y) -> [y]
  (Just x, Nothing) -> [x]
  (Just x, Just y) -> if x == y then [x] else [x, y]
  where
    long = getLongOptionOffset s
    short = getShortOptionOffset s

----------------------------------------
-- For 3-pane layout (short-option   long-option   description)

-- | Get location of long options
getLongOptionLocations :: String -> [Location]
getLongOptionLocations = _getNonblankLocationTemplate Utils.startsWithDoubleDash

-- | Get location of short options
getShortOptionLocations :: String -> [Location]
getShortOptionLocations = _getNonblankLocationTemplate Utils.startsWithSingleDash

-- | Get estimated horizontal offset of long options
getLongOptionOffset :: String -> Maybe Int
getLongOptionOffset = _getOffsetHelper getLongOptionLocations

-- | Get estimated horizontal offset of short options
getShortOptionOffset :: String -> Maybe Int
getShortOptionOffset = _getOffsetHelper getShortOptionLocations

-- | Helper: get a frequency-based estimate of horizontal offset
_getOffsetHelper :: (String -> [Location]) -> String -> Maybe Int
_getOffsetHelper getLocs s = traceMessage res
  where
    locs = getLocs s
    offsets = map snd locs
    res = getMostFrequent offsets
    droppedOptionLinesInfo = unlines [(printf "dropped lines: (%03d) %s" r (lines s !! r) :: String) | (r, c) <- locs, Just c /= res]
    traceMessage = Utils.infoTrace droppedOptionLinesInfo

--------------------------------------------------------------------------------
-- Offset Estimation
--------------------------------------------------------------------------------

-- | Returns the estimate of description offset after looking at all lines
-- AND option locations that failed to satisfy the layout.
--
-- There are two independent ways to guess the horizontal offset of descriptions:
--
--   1. A description line may be simply indented by space
--   2. A description line may appear following an option
--      (from the pattern that the description and the options+args
--      may be separated by 3 or more spaces)
--
-- Returns Nothing if 1 and 2 disagree, or no information in 1 and 2.
--
-- The 75% alignment threshold is used to determine if the layout is valid:
-- we require that at least 75% of option lines align with the detected
-- description offset. This is permissive enough to handle real-world
-- variations while still being a strong signal.
getDescOffsetEstimate :: Int -> String -> [Location] -> (Maybe Int, [Location])
getDescOffsetEstimate lineIdxBase s optLocs =
  case descOffsetWithCountInNonoptLines lineIdxBase s optLocs of
    (Nothing, optLocsRemoved) ->
      case descOffsetWithCountInOptionLines lineIdxBase s (filter (`notElem` optLocsRemoved) optLocs) of
        Nothing -> Utils.infoTrace "No layout information found" (Nothing, optLocsRemoved)
        Just (x2, c2) ->
          if isAlignedMoreThan75Percent c2
            then Utils.infoTrace "Descriptions always appear in the lines with options" (Just x2, optLocsRemoved)
            else Utils.infoTrace "Layout analysis yielded no results" (Nothing, optLocsRemoved)
    (Just (x1, c1), optLocsRemoved) ->
      case descOffsetWithCountInOptionLines lineIdxBase s (filter (`notElem` optLocsRemoved) optLocs) of
        Nothing -> Utils.infoTrace "Descriptions never appear in the lines with options" (Just x1, optLocsRemoved)
        Just (x2, c2)
          | x1 == x2 -> (Just x1, optLocsRemoved)
          -- If count from option lines is high (>3) and well-aligned (>=75%),
          -- trust it over low-count description-only line estimates
          | c1 <= 3 && 3 < c2 && isAlignedMoreThan75Percent c2 -> (debug Just x2, optLocsRemoved)
          -- If count from description-only lines is high, trust it
          | c2 <= 3 && 3 < c1 -> (debug Just x1, optLocsRemoved)
          -- If offsets differ by <5 columns and option-line method is well-aligned,
          -- trust option-line method (description-only lines may have extra indent)
          | 0 < x1 - x2 && x1 - x2 < 5 && isAlignedMoreThan75Percent c2 -> (debug (Just x2), optLocsRemoved)
          | otherwise -> (debug Nothing, optLocsRemoved)
          where
            msg =
              "Disagreement in offsets:\n\
              \   description-only-line offset   %d (with count %d)\n\
              \   option+description-line offset %d (with count %d)\n"
            debug = Utils.warnTrace (printf msg x1 c1 x2 c2 :: String)
  where
    -- The 75% threshold: require that at least 75% of option lines align
    -- with the detected description offset. This allows for noise while
    -- still requiring a strong signal.
    isAlignedMoreThan75Percent c = c * 100 >= 75 * length optLocs

-- | Estimate offset of description in non-option lines.
--   Returns (Just (description offset, match count), [removed option locations]) if matches
descOffsetWithCountInNonoptLines :: Int -> String -> [Location] -> (Maybe (Int, Int), [Location])
descOffsetWithCountInNonoptLines lineIdxBase s optLocs
  | null offsetOverlaps = (res, [])
  | otherwise = (res, optLocsRemoved)
  where
    xs = lines s
    typeMetadataRows = Set.fromList $ getTypeMetadataRowsAfterOptions xs optLocs
    descLocs =
      Utils.infoShowCoords "Description locations:" lineIdxBase $
        nubSort $
          getTypeAwareDescriptionLocations xs optLocs
            ++ takeHangingDesc lineIdxBase optLocs
              ( filter
                  (\(row, _) -> row `Set.notMember` typeMetadataRows && not (isStatusAnnotationLine (xs !! row)))
                  (getNonoptLocations s)
              )
    (_, descOffsets) = unzip descLocs
    (optLines, optOffsets) = unzip optLocs
    offsetOverlaps = Set.toList $ Set.intersection (Set.fromList optOffsets) (Set.fromList descOffsets)
    (optOffsets', optOffsetsRemoved) = span (< 10) optOffsets
    optLocsRemoved =
      Utils.infoShowCoords "Removed option locations:" lineIdxBase $
        filter (\(_, c) -> c `elem` optOffsetsRemoved) optLocs
    indentations =
      infoMsg "Description indentations:" $
        [ x
          | (r, x) <- descLocs,
            -- description's offset is equal (rare case!) or greater than option's
            null optOffsets' || (List.maximum optOffsets' <= x),
            -- description can exist only around option lines
            isAroundOptionLines r
            -- previous line cannot be blank
        ]
    res = infoMsg "Description offset (from non-option lines):" $ getMostFrequentWithCount indentations

    isAroundOptionLines r =
      case optLines of
        [] -> False
        firstOptLine : restOptLines ->
          let lastOptLine = foldl max firstOptLine restOptLines
           in firstOptLine < r && r < lastOptLine + 5

-- | Take description locations that hang from an option line.
--
-- Consider the following empty-line delimited patterns:
--
-- @
--    Heading                                                       \<--- NOT hanging
--      --option arg   description
--                     continued description                        \<--- hanging
--
--    Another heading                                               \<--- NOT hanging
--      --option arg
--           description                                            \<--- hanging
--
--      --option (arg) Somehow explanation immediately follows
--      and it's continued to the next lines without indentation.   \<--- hanging
--
--         --option arg
--      When following description lines are indented less than     \<--- NOT hanging
--      the option line itself, they are not hanging.               \<--- NOT hanging
--
--      Some descriptions are not tied to particular options, yet show    \<--- NOT hanging
--      --option in the middle of the sentences. This case should be
--      excluded from description statistics.                             \<--- NOT hanging
--
--    Something blah                           \<--- NOT hanging
--      more blah...                           \<--- NOT hanging
-- @
takeHangingDesc :: Int -> [Location] -> [Location] -> [Location]
takeHangingDesc lineIdxBase optLocs descLocs = descLocSelected
  where
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

-- | Estimate offset of description part from the lines with options.
-- Returns Just (offset size, match count) if matches.
--
-- Uses the 3-space column separator heuristic: most CLI tools use at least
-- 3 spaces to separate options from descriptions. Single/double spaces
-- commonly appear within option arguments (e.g., @-o FILE@).
descOffsetWithCountInOptionLines :: Int -> String -> [Location] -> Maybe (Int, Int)
descOffsetWithCountInOptionLines _ s optLocs =
  infoMsg "Description offset (from option lines):" res
  where
    -- Hardcoded 3-space separator: most CLI tools use at least 3 spaces
    -- between options and descriptions. Single/double spaces appear within
    -- option arguments (e.g., "-o FILE"), so 3+ spaces is a reliable boundary.
    sep = "   "
    xs = lines s
    optLines = map ((xs !!) . fst) optLocs
    xss = map (fst . breakOnEnd sep . trimEnd) optLines
    res =
      getMostFrequentWithCount $
        map length $
          filter (not . null . trim) xss

--------------------------------------------------------------------------------
-- String Inspection Helpers
--------------------------------------------------------------------------------

-- | Check if a word starting with space indentation.
isWordStartingWithIndentation :: Int -> String -> Bool
isWordStartingWithIndentation _ "" = False
isWordStartingWithIndentation n x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    condBefore && condAfter
  where
    isSpacesOnly s = (not . null) s && (null . trim) s
    (before, after) = splitAt n x
    condBefore = isSpacesOnly before
    condAfter =
      case after of
        [] -> False
        c : _ -> c /= ' '

-- | Check if a word starting around the horizontal position.
-- Ambiguity is set by margin value.
isWordStartingAround :: Int -> Int -> String -> Bool
isWordStartingAround _ _ "" = False
isWordStartingAround margin offset x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    any (`isWordStartingAt` x) indices
  where
    indices = [offset .. offset + margin]

-- | Similar to isWordStartingAround, but a dash-prefixed character sequence is not considered as a word
isNonDashWordStartingAround :: Int -> Int -> String -> Bool
isNonDashWordStartingAround _ _ "" = False
isNonDashWordStartingAround margin offset x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    any (\idx -> isWordStartingAt idx x && (x !! idx) /= '-') indices
  where
    indices = [offset .. offset + margin]

isWordStartingAt :: Int -> String -> Bool
isWordStartingAt offset x =
  (not . null . trim) before && (not . null . trim) after && hasWordBoundary
  where
    (before, after) = splitAt offset x
    hasWordBoundary =
      case (reverse before, after) of
        (' ' : _, c : _) -> c /= ' '
        _ -> False

-- | QIIME/Click can wrap rich type information between the option name and
-- the real description:
--
-- @
--   --i-sequences ARTIFACT FeatureData[Sequence]¹ |
--     FeatureData[ProteinSequence]²
--                           The sequences to be aligned.
--   --p-maxiterate INTEGER  Specifies how many iterative refinement cycles are
--     Range(0, None)        performed after the initial progressive alignment.
-- @
--
-- The completion only needs the placeholder argument (ARTIFACT, INTEGER,
-- TEXT, ...), while the bracketed/ranged metadata should neither become the
-- description offset nor be emitted as description text.
looksLikeTypeMetadata :: String -> Bool
looksLikeTypeMetadata s =
  not (null trimmed)
    && not (Utils.startsWithDash trimmed)
    && last trimmed /= '.'
    && (looksLikeShortTypeMetadata || looksLikeChoiceListStart trimmed || looksLikeChoiceListContinuation trimmed)
  where
    trimmed = trim s
    ws = words trimmed
    looksLikeShortTypeMetadata =
      not (null ws)
        && length ws <= 2
        && any (`elem` trimmed) ("[](){}'\",|" :: String)

looksLikeChoiceListStart :: String -> Bool
looksLikeChoiceListStart s =
  "Choices(" `List.isPrefixOf` s
    && ("," `isInfixOf` s || ")" `List.isSuffixOf` s)
    && all isChoiceListChar s

looksLikeChoiceListContinuation :: String -> Bool
looksLikeChoiceListContinuation "" = False
looksLikeChoiceListContinuation s@(first : _) =
  first `elem` ("'\"" :: String)
    && ("," `isInfixOf` s || ")" `List.isSuffixOf` s)
    && all isChoiceListChar s

isChoiceListChar :: Char -> Bool
isChoiceListChar c =
  isAlphaNum c || c `elem` (" '\"`,()_-./" :: String)

isOptionLineWithOnlyMetadata :: Int -> String -> Bool
isOptionLineWithOnlyMetadata offset line =
  looksLikeTypeMetadata (drop offset line)

isStatusAnnotationLine :: String -> Bool
isStatusAnnotationLine line =
  case trim line of
    "[required]" -> True
    "[optional]" -> True
    s -> "[default:" `List.isPrefixOf` s && "]" `List.isSuffixOf` s

metadataDescriptionOffset :: String -> Maybe Int
metadataDescriptionOffset line =
  Maybe.listToMaybe
    [ end
    | (start, end) <- spaceRuns line
    , end < length line
    , let prefix = trim (take start line)
    , looksLikeTypeMetadata prefix
    , not (null (trim (drop end line)))
    ]
  where
    spaceRuns x =
      [ (start, end)
      | start <- [0 .. length x - 1]
      , x !! start == ' '
      , start == 0 || x !! (start - 1) /= ' '
      , let end = start + length (takeWhile (== ' ') (drop start x))
      , end - start >= 3
      ]

isMetadataDescriptionLine :: Int -> String -> Bool
isMetadataDescriptionLine offset line = metadataDescriptionOffset line == Just offset

isMetadataOnlyLine :: String -> Bool
isMetadataOnlyLine line =
  not (isStatusAnnotationLine line)
    && looksLikeTypeMetadata line
    && Maybe.isNothing (metadataDescriptionOffset line)

isTypeMetadataLine :: String -> Bool
isTypeMetadataLine line =
  isMetadataOnlyLine line || Maybe.isJust (metadataDescriptionOffset line)

getTypeMetadataRowsAfterOptions :: [String] -> [Location] -> [Int]
getTypeMetadataRowsAfterOptions xs optLocs = concatMap rowsAfterOpt optLocs
  where
    optLinesSet = Set.fromList (map fst optLocs)

    rowsAfterOpt (row, optIndent) = go (row + 1)
      where
        go idx
          | idx >= length xs = []
          | idx `Set.member` optLinesSet = []
          | null (trim line) = []
          | indentation <= optIndent = []
          | isTypeMetadataLine line = idx : go (idx + 1)
          | otherwise = []
          where
            line = xs !! idx
            indentation = length (takeWhile (== ' ') line)

getTypeAwareDescriptionLocations :: [String] -> [Location] -> [Location]
getTypeAwareDescriptionLocations xs optLocs = concatMap descLocsAfterOpt optLocs
  where
    optLinesSet = Set.fromList (map fst optLocs)

    descLocsAfterOpt (row, optIndent) = go False (row + 1)
      where
        go seenMetadata idx
          | idx >= length xs = []
          | idx `Set.member` optLinesSet = []
          | null (trim line) = []
          | indentation <= optIndent = []
          | isStatusAnnotationLine line = []
          | Just offset <- metadataDescriptionOffset line = (idx, offset) : go True (idx + 1)
          | isMetadataOnlyLine line = go True (idx + 1)
          | seenMetadata && indentation >= optIndent + 6 = (idx, indentation) : go True (idx + 1)
          | otherwise = []
          where
            line = xs !! idx
            indentation = length (takeWhile (== ' ') line)

splitAfter :: Int -> String -> (String, String)
splitAfter offset x
  | null found = (x, "")
  | otherwise =
      case found of
        pair : _ -> pair
        [] -> (x, "")
  where
    indices = [offset .. length x - 1]
    found =
      [ splitAt i x
        | i <- indices,
          let ch = x !! (i - 1),
          i == 0 || ch `elem` " }]>"
      ]

--------------------------------------------------------------------------------
-- Main Layout Analysis
--------------------------------------------------------------------------------

-- | Returns option line's
-- (1) consensus beginning of description
-- (2) option locations [(row, col)]. All in 0-based indexing.
--
-- [TODO] Should attempt more when descriptionOffsetMay is Nothing.
getDescOffsetOptLocsPair :: Int -> String -> (Maybe Int, [(Int, Int)])
getDescOffsetOptLocsPair lineIdxBase s
    | null optionOffsets = Utils.infoTrace "No option offsets found" (Nothing, [])
    | otherwise = case descriptionOffsetMay of
        Nothing ->
            Utils.infoTrace "Description offset could not be determined" (Nothing, optLocsFixed)
        Just descOffset
            | descOffset <= 3 ->
                Utils.infoTrace "Description offset too small (<=3)" (Nothing, optLocsFixed)
            | null optLocsFixed ->
                Utils.infoTrace "No option locations found" (Nothing, [])
            | otherwise ->
                Utils.infoTrace "Description offset and option locations:" (Just descOffset, optLocsFixed)
  where
    optionOffsets = infoMsg "Option offsets:" $ getOptionOffsets s
    optLocsCandidates = getOptionLocations s

    (optLocs, _) = List.partition (\(_, c) -> c `elem` optionOffsets) optLocsCandidates

    -- Deconstruct the tuple once
    (descriptionOffsetMay, optLocsRemoved) =
        getDescOffsetEstimate lineIdxBase s $
            Utils.infoShowCoords "Option locations:" lineIdxBase optLocs

    optLocsFixed =
        Utils.infoShowCoords "Fixed option locations:" lineIdxBase $
            filter (`notElem` optLocsRemoved) optLocs

-- | Get description offset from text (convenience function)
getDescriptionOffset :: String -> Maybe Int
getDescriptionOffset s = fst $ getDescOffsetOptLocsPair 0 s

-- | Returns option-description pairs based on layouts
-- AND the option locations uncaught in the process.
getOptionDescriptionPairsFromLayout :: Int -> String -> Maybe ([(String, String)], [Location])
getOptionDescriptionPairsFromLayout lineIdxBase s
  | Maybe.isNothing descOffsetMay || null res = Nothing
  | otherwise = Just (res, Utils.infoShowCoords "Dropped option locations:" lineIdxBase droppedOptLocs)
  where
    (descOffsetMay, optLocs) = getDescOffsetOptLocsPair lineIdxBase s
    descOffset = Maybe.fromJust descOffsetMay
    xs = lines s
    optLines = map fst optLocs
    optLinesSet = Set.fromList optLines
    -- More accommodating description line matching seems to work better...
    descLinesWithoutOptions =
      Utils.infoShowIndices
        "descLinesWithoutOptions:"
        lineIdxBase
        [ idx
        | (idx, x) <- zip [0 ..] xs
        , idx `Set.notMember` optLinesSet
        , isDescriptionLineWithoutOption idx x
        ]
    linewidths = map (length . (xs !!)) descLinesWithoutOptions
    descLineWidthTop10Percentile =
      infoMsg "Description line width (90th percentile):" $
        Maybe.fromMaybe 80 (Utils.topTenPercentile linewidths)
    descLinesWithoutOptionsSet = Set.fromList descLinesWithoutOptions

    isDescriptionLineWithoutOption idx line =
      isWordStartingWithIndentation descOffset line
        || isMetadataDescriptionLine descOffset line
        || isMetadataContinuationLine idx line
        || isStatusContinuationLine idx line

    isMetadataContinuationLine idx line =
      isMetadataOnlyLine line
        && idx > 0
        && ((idx - 1) `Set.member` optLinesSet || isTypeMetadataLine (xs !! (idx - 1)))

    isStatusContinuationLine idx line =
      isStatusAnnotationLine line
        && idx > 0
        && canContinueStatus (xs !! (idx - 1))

    canContinueStatus line =
        isStatusAnnotationLine line
          || Utils.startsWithDash line
          || isWordStartingWithIndentation descOffset line
          || isMetadataDescriptionLine descOffset line
          || isMetadataOnlyLine line

    -- The line must be long when description starts at the same line
    -- the option and continues to the next line.
    -- [FIXME] too heuristic
    isOptionAndDescriptionLine idx
      | not (isOptionLine idx) = False
      | isOptionLineWithOnlyMetadata descOffset x = False
      | length xs == idx + 1 = True
      | isOptionLine (idx + 1) =
          -- When both current and the next lines have options
          isSplittingRoughly
            && ( ( descOffset >= 2
                     && (not . null) optSegment
                     && (length . words . trim) descSegment >= 2
                 )
                   || hasSpacesAtMiddle x
                   || isParsedAsOptDescLine x
                     && length x + 25 > descLineWidthTop10Percentile
               )
      | otherwise =
          -- When current one has an option, but NOT the next line
          isSplittingNearly
            && ( (not . isDescriptionOnly) (idx + 1)
                   || length x + 6 > descLineWidthTop10Percentile
                   || hasSpacesAtMiddle x
                   || isParsedAsOptDescLine x
                     && length x + 25 > descLineWidthTop10Percentile
               )
      where
        x = xs !! idx
        isOptionLine i = i `Set.member` optLinesSet
        isDescriptionOnly i = i `Set.member` descLinesWithoutOptionsSet
        (optSegment, descSegment) = splitAt descOffset x
        isParsedAsOptDescLine = not . null . HelpParser.parseLine
        hasSpacesAtMiddle = ("   " `isInfixOf`) . trim
        isSplittingNearly = isNonDashWordStartingAround 2 descOffset x
        isSplittingRoughly = isWordStartingAround 8 descOffset x

    descLinesWithOptions =
      Utils.infoShowIndices
        "descLinesWithOptions:"
        lineIdxBase
        [idx | idx <- optLines, isOptionAndDescriptionLine idx]
    descLines =
      Utils.infoShowIndices "descLines:" lineIdxBase $
        nubSort (descLinesWithoutOptions ++ descLinesWithOptions)

    (quartets, droppedOptLines) = toConsecutiveRangeQuartets optLines descLines
    droppedOptLocs = filter (\(x, _) -> x `elem` droppedOptLines) optLocs
    quartetsMod = Utils.infoShowQuartets "quartets:" lineIdxBase $ [(a, b, updateDescFrom xs descOffset a c, d) | (a, b, c, d) <- quartets] -- [(optFrom, optTo, descFrom, descTo)]
    res = concatMap (handleQuartet xs descOffset) quartetsMod

--------------------------------------------------------------------------------
-- Text Extraction
--------------------------------------------------------------------------------

-- | Returns option-description pairs based on description's offset value + quartet
-- lineStr :: [String]
-- descriptionOffset :: Int
-- (optionLineIndexFrom, optionLineIndexTo, descriptionLineIndexFrom, descriptionLineIndexTo)
-- where [from, to) is half-open range
handleQuartet :: [String] -> Int -> (Int, Int, Int, Int) -> [(String, String)]
handleQuartet xs offset (optFrom, optTo, descFrom, descTo)
  | optFrom == descFrom && optTo == descTo = onelinersF optFrom optTo
  | optFrom == descFrom = onelinersF optFrom (optTo - 1) ++ [squashDescSideF (optTo - 1) descTo]
  | optTo == descFrom = [squashOptionsAndDescriptionsNoOverlapF optFrom descFrom descTo]
  | optTo == descTo = squashOptsF optFrom (descFrom + 1) : onelinersF (descFrom + 1) descTo
  | optTo - 1 == descFrom = [squashOptionsAndDescriptionsOverlapF optFrom optTo descTo]
  | otherwise = (s1 : ss) ++ [s2]
  where
    squashOptsF a b = squashOptionsAndDescriptionsOverlap xs offset a b b
    squashDescSideF a b = squashOptionsAndDescriptionsOverlap xs offset a (a + 1) b
    onelinersF = oneliners xs offset
    squashOptionsAndDescriptionsOverlapF = squashOptionsAndDescriptionsOverlap xs offset
    squashOptionsAndDescriptionsNoOverlapF = squashOptionsAndDescriptionsNoOverlap xs offset
    s1 = squashOptsF optFrom (descFrom + 1)
    ss = onelinersF (descFrom + 1) (optTo - 1)
    s2 = squashDescSideF (optTo - 1) descTo

squashOptionsAndDescriptionsNoOverlap :: [String] -> Int -> Int -> Int -> Int -> (String, String)
squashOptionsAndDescriptionsNoOverlap xs offset a b c = (opt, desc)
  where
    optLines = map (trim . (xs !!)) $ take (b - a) [a, a + 1 ..]
    opt = List.intercalate "," optLines
    descLines = map (descriptionSegment offset . (xs !!)) $ take (c - b) [b, b + 1 ..]
    desc = unlines descLines

squashOptionsAndDescriptionsOverlap :: [String] -> Int -> Int -> Int -> Int -> (String, String)
squashOptionsAndDescriptionsOverlap xs offset a b c = (opt, desc)
  where
    optLines = map (xs !!) $ take (b - a) [a, a + 1 ..]
    optLinesLastTruncated = map trim (init optLines ++ [(fst . splitAfter offset . last) optLines])
    opt = List.intercalate "," optLinesLastTruncated
    descLines = map (descriptionSegment offset . (xs !!)) $ take (c - b + 1) [b - 1, b ..]
    desc = unlines descLines

descriptionSegment :: Int -> String -> String
descriptionSegment offset line
  | isStatusAnnotationLine line = trim line
  | isWordStartingWithIndentation offset line = snd (splitAt offset line)
  | isMetadataDescriptionLine offset line = snd (splitAt offset line)
  | isMetadataOnlyLine line = ""
  | otherwise = snd (splitAfter offset line)

oneliners :: [String] -> Int -> Int -> Int -> [(String, String)]
oneliners xs offset a b =
  [ (trim former, latter)
    | i <- take (b - a) [a, a + 1 ..],
      let x = xs !! i,
      let (former, latter)
            | isWordStartingAt offset x = splitAt offset x
            | pair : _ <- pairs = pair
            | otherwise = splitAfter offset x
            where
              pairs = map fst (readP_to_S HelpParser.preprocessor x)
  ]

updateDescFrom :: [String] -> Int -> Int -> Int -> Int
updateDescFrom xs offset optFrom descFrom
  | null ys = descFrom
  | otherwise = debugMsg "Updated description from:" res
  where
    indices = take (optFrom - descFrom) [descFrom - 1, descFrom - 2 ..]
    ys = takeWhile (\i -> isWordStartingAround 2 offset (xs !! i)) indices
    res = last ys

--------------------------------------------------------------------------------
-- Range Operations
--------------------------------------------------------------------------------

-- | Returns (optFrom, optTo, descFrom, descTo) quartets
-- AND the dropped line indices in xs
toConsecutiveRangeQuartets :: [Int] -> [Int] -> ([(Int, Int, Int, Int)], [Int])
toConsecutiveRangeQuartets xs ys =
  (res, droppedOptLines)
  where
    (xRanges, yRanges) = makeRangePair xs ys
    res = mergeRange xRanges yRanges
    resXRanges = [(x1, x2) | (x1, x2, _, _) <- res]
    droppedOptLines = filter (not . Utils.contains resXRanges) xs

-- | Make pairs of overlapping ranges.
--
-- When two ranges (x1, x2) and (y1, y2) overlap,
-- they must satisfy x1 <= y1 <= x2 <= y2.
-- When x2 == y1, its still treated as "overlap"
-- although [x1, x2) and [y1, y2) have empty intersection.
--
-- [NOTE] this can drop some items in xs (after `last yEnds)
makeRangePair :: [Int] -> [Int] -> ([Range], [Range])
makeRangePair xs ys =
  (xRanges, yRanges)
  where
    xStarts = map fst (Utils.toRanges xs)
    yEnds = map snd (Utils.toRanges ys)
    (xssHead, xss) = List.mapAccumR f xs yEnds
    f acc y = span (< y) acc
    xRanges = concatMap Utils.toRanges (xssHead : if null xss then [] else init xss)
    (_, yss) = List.mapAccumR g ys xStarts
    g acc x = span (< x) acc
    yRanges = concatMap Utils.toRanges yss

-- | Create quartets (x1, x2, y1, y2) as overlapping boundaries.
--
-- [Note] As a special case, x2 == y1 is considered as an overlap
-- although [x1, x2) and [y1, y2) have empty intersection.
mergeRange :: [Range] -> [Range] -> [(Int, Int, Int, Int)]
mergeRange _ [] = []
mergeRange [] _ = []
mergeRange ((x1, x2) : xs) ((y1, y2) : ys)
  | x2 < y1 = mergeRange xs ((y1, y2) : ys)
  | y2 <= x1 = mergeRange ((x1, x2) : xs) ys
  | otherwise = assert cond $ (x1, x2, y1, y2) : mergeRange xs ys
  where
    cond = x1 <= y1 && y1 <= x2 && x2 <= y2
