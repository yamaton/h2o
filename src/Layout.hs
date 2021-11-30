{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Layout where

import Control.Exception (assert)
import qualified Data.List as List
import Data.List.Extra (nubSort, trim, trimEnd, splitOn)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Debug.Trace (trace)
import HelpParser (parseLine, parseWithOptPart, preprocessAllFallback)
import Text.Printf (printf)
import Type (Opt)
import Utils (debugMsg, debugShow, getMostFrequent, getMostFrequentWithCount, getParagraph, infoMsg, infoShow, smartUnwords, toRanges, warnShow)
import qualified Utils

-- | Location is defined by (row, col) order
type Location = (Int, Int)

-- [TODO] memoise the calls
-- https://stackoverflow.com/questions/3208258/memoization-in-haskell

-- | a helper function
_getNonblankLocationTemplate :: (String -> Bool) -> String -> [Location]
_getNonblankLocationTemplate f s = [(i, getHorizOffset x) | (i, x) <- enumLines, f x]
  where
    enumLines = zip [(0 :: Int) ..] (lines s)
    getHorizOffset = length . takeWhile (== ' ')

-- | Get location that starts with '-'
-- λ> getOptionLocations " \n\n  \t  --option here"
-- [(2, 10)]
getOptionLocations :: String -> [Location]
getOptionLocations = _getNonblankLocationTemplate Utils.startsWithDash

-- | Get locations of lines NOT starting with dash
getNonoptLocations :: String -> [Location]
getNonoptLocations = _getNonblankLocationTemplate (not . Utils.startsWithDash)

_getOffsetHelper :: (String -> [Location]) -> String -> Maybe Int
_getOffsetHelper getLocs s = traceMessage res
  where
    locs = getLocs s
    xs = map snd locs
    res = getMostFrequent xs
    droppedOptionLinesInfo = unlines [(printf "[info] layout: dropped lines: (%03d) %s" r (lines s !! r) :: String) | (r, c) <- locs, Just c /= res]
    traceMessage = trace droppedOptionLinesInfo

-- | get presumed horizontal offsets of options lines
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

-- | get location of long options
getLongOptionLocations :: String -> [Location]
getLongOptionLocations = _getNonblankLocationTemplate Utils.startsWithDoubleDash

-- | get location of long options
getShortOptionLocations :: String -> [Location]
getShortOptionLocations = _getNonblankLocationTemplate Utils.startsWithSingleDash

-- | get presumed horizontal offset of long options
getLongOptionOffset :: String -> Maybe Int
getLongOptionOffset = _getOffsetHelper getLongOptionLocations

-- | get presumed horizontal offset of long options
getShortOptionOffset :: String -> Maybe Int
getShortOptionOffset = _getOffsetHelper getShortOptionLocations

----------------------------------------

-- There are two independent ways to guess the horizontal offset of descriptions
--   1) A description line may be simply offset by space
--   2) A description line may appear following an option
--      	... from the pattern that the description and the options+args
--      	may be separated by 3 or more spaces
-- Returns Nothing if 1 and 2 disagrees, or no information in 1 and 2
--
getDescriptionOffsetFromOptionLocs :: String -> [Location] -> (Maybe Int, [Location])
getDescriptionOffsetFromOptionLocs s optLocs =
  case descOffsetWithCountSimple s optLocs of
    (Nothing, optLocsRemoved) ->
      case descOffsetWithCountInOptionLines s (filter (`notElem` optLocsRemoved) optLocs) of
        Nothing -> trace "[info] Retrieved absolutely zero information" (Nothing, optLocsRemoved)
        Just (x2, c2) ->
          if isAlignedMoreThan80Percent c2
            then Utils.infoTrace "Descriptions always appear in the lines with options" (Just x2, optLocsRemoved)
            else Utils.infoTrace "Retrieved nothing from layout" (Nothing, optLocsRemoved)
    (Just (x1, c1), optLocsRemoved) ->
      case descOffsetWithCountInOptionLines s (filter (`notElem` optLocsRemoved) optLocs) of
        Nothing -> trace "[info] Descriptions never appear in the lines with options" (Just x1, optLocsRemoved)
        Just (x2, c2)
          | x1 == x2 -> (Just x1, optLocsRemoved)
          | c1 <= 3 && 3 < c2 && isAlignedMoreThan80Percent c2 -> (debug Just x2, optLocsRemoved)
          | c2 <= 3 && 3 < c1 -> (debug Just x1, optLocsRemoved)
          | 0 < x1 - x2 && x1 - x2 < 5 && isAlignedMoreThan80Percent c2 -> (debug (Just x2), optLocsRemoved) -- sometimes continued lines are indented.
          | otherwise -> (debug Nothing, optLocsRemoved)
          where
            msg =
              "[warn] Disagreement in offsets:\n\
              \   description-only-line offset   %d (with count %d)\n\
              \   option+description-line offset %d (with count %d)\n"
            debug = trace (printf msg x1 c1 x2 c2 :: String)
  where
    isAlignedMoreThan80Percent c = c * 10 >= 8 * length optLocs

-- | Estimate offset of description part from non-option lines.
-- | Returns Just (offset size, match count) if matches
descOffsetWithCountSimple :: String -> [Location] -> (Maybe (Int, Int), [Location])
descOffsetWithCountSimple s optLocs
  | null offsetOverlaps = (res, [])
  | otherwise = (res, optLocsRemoved)
  where
    descLocs = getNonoptLocations s
    (_, descOffsets) = unzip descLocs
    (optLineNums, optOffsets) = unzip optLocs
    offsetOverlaps = Set.toList $ Set.intersection (Set.fromList optOffsets) (Set.fromList descOffsets)
    descOverlapCounts = map (\overlap -> length . filter (== overlap) $ descOffsets) offsetOverlaps
    optOverlapCounts = map (\overlap -> length . filter (== overlap) $ optOffsets) offsetOverlaps
    optOffsetsRemoved =
      [ offset | (offset, optCount, descCount) <- zip3 offsetOverlaps optOverlapCounts descOverlapCounts, optCount <= descCount, 10 <= offset
      ]
    optOffsets' = filter (`notElem` optOffsetsRemoved) optOffsets
    optLocsRemoved = infoMsg "[info] optLocsRemoved: " $ filter (\(_, c) -> c `elem` optOffsetsRemoved) optLocs
    cols =
      [ x | (r, x) <- descLocs,
            -- description's offset is equal (rare case!) or greater than option's
            null optOffsets' || (List.maximum optOffsets' < x),
            -- description can exist only around option lines
            head optLineNums < r, r < last optLineNums + 5
            -- previous line cannot be blank
      ]
    res = getMostFrequentWithCount cols

-- | Estimate offset of description part from the lines with options
-- | Returns Just (offset size, match count) if matches
descOffsetWithCountInOptionLines :: String -> [Location] -> Maybe (Int, Int)
descOffsetWithCountInOptionLines s optLocs =
  assert ('\t' `notElem` s) res
  where
    sep = "   " -- hardcoded as 3 spaces for now
    xs = lines s
    -- reversed to handle spacing not multiples of 3
    -- for example `splitOn sep "--opt     desc"` == ["--opt", "  desc"]`
    -- but I don't want spaces at the beginning of the description
    optLineNums = map fst optLocs
    xss = map (List.intercalate sep . tail . splitOn sep . reverse . trimEnd . (xs !!)) optLineNums
    res = getMostFrequentWithCount $ map ((n +) . length) $ filter (not . isSpacesOnly) $ filter (not . null) xss
      where
        n = length sep

isSpacesOnly :: String -> Bool
isSpacesOnly s = not (null s) && all (' ' ==) s

isWordStartingAtOffsetAfterBlank :: Int -> String -> Bool
isWordStartingAtOffsetAfterBlank _ "" = False
isWordStartingAtOffsetAfterBlank n x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    condBefore && condAfter
  where
    (before, after) = splitAt n x
    condBefore = isSpacesOnly before
    condAfter = not (null after) && head after /= ' '

isWordStartingAtOffset :: Int -> String -> Bool
isWordStartingAtOffset _ "" = False
isWordStartingAtOffset 0 (c : _) = c /= ' '
isWordStartingAtOffset n x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    not (null before) && not (null after) && last before == ' ' && head after /= ' '
  where
    (before, after) = splitAt n x

isWordStartingAround :: Int -> Int -> String -> Bool
isWordStartingAround _ _ "" = False
isWordStartingAround margin idx x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    any criteria indices
  where
    indices = [idx - margin, idx - margin + 1 .. idx + margin]
    criteria i =
      not (null before) && not (null after) && last before == ' ' && head after /= ' '
      where
        (before, after) = splitAt i x

isSeparatedAtOffset :: Int -> String -> String -> Bool
isSeparatedAtOffset _ _ "" = False
isSeparatedAtOffset n sep x
  | n <= w || length x <= w = False
  | otherwise =
    assert ('\n' `notElem` x && '\t' `notElem` x) $
      sep `List.isSuffixOf` before && (not (null after) && head after /= ' ')
  where
    w = length sep
    (before, after) = splitAt n x

-- ================================================
-- ============== Main stuff ======================

getDescriptionOffsetOptLineNumsPair :: String -> Maybe (Int, [Int])
getDescriptionOffsetOptLineNumsPair s
  | null optionOffsets || Maybe.isNothing descriptionOffsetMay = Nothing
  | offset <= 3 || null optLineNumsFixed = Nothing
  | otherwise = Just (offset, optLineNumsFixed)
  where
    optionOffsets = infoMsg "layout: Option offsets:" $ getOptionOffsets s
    optLocsCandidates = getOptionLocations s
    (optLocs, optLocsExcluded) = List.partition (\(_, c) -> c `elem` optionOffsets) optLocsCandidates
    optLineNums = debugShow "layout: optLocsExcluded:" optLocsExcluded $ infoMsg "optLineNums" $ map fst optLocs

    (descriptionOffsetMay, optLocsRemoved) = getDescriptionOffsetFromOptionLocs s optLocs
    offset = infoMsg "layout: Description offset:" $ Maybe.fromJust descriptionOffsetMay
    optLineNumsFixed = infoMsg "layout: optLineNumsFixed" $ filter (`notElem` map fst optLocsRemoved) optLineNums

getDescriptionOffset :: String -> Maybe Int
getDescriptionOffset s = fst <$> getDescriptionOffsetOptLineNumsPair s

-- | Returns option-description pairs based on layouts AND also returns the dropped
-- line index ranges that is uncaught in the process.
getOptionDescriptionPairsFromLayout :: String -> Maybe ([(String, String)], [(Int, Int)])
getOptionDescriptionPairsFromLayout s
  | Maybe.isNothing tupMay || null res = Nothing
  | otherwise = Just $ infoShow "Dropped option indices:" dropped (res, dropped)
  where
    tupMay = getDescriptionOffsetOptLineNumsPair s
    (offset, optLineNums) = Maybe.fromJust tupMay
    xs = lines s
    optLineNumsSet = Set.fromList optLineNums
    -- More accomodating description line matching seems to work better...
    descLineNumsWithoutOption =
      debugMsg
        "descLineNumsWithoutOption"
        [ idx | (idx, x) <- zip [0 ..] xs, isWordStartingAtOffsetAfterBlank offset x, idx `Set.notMember` optLineNumsSet
        ]
    linewidths = map (length . (xs !!)) descLineNumsWithoutOption
    descriptionLineWidthTop10Percent = infoMsg "descriptionLineLength at 93%: " $ if null linewidths then 80 else Utils.topTenPercentile linewidths
    descLineNumsWithoutOptionSet = Set.fromList descLineNumsWithoutOption

    -- The line must be long when description starts at the option line and continues to the next line.
    -- Here I mean "long" by the
    isOptionAndDescriptionLine idx
      | not (isOptionLine idx) = False
      | otherwise =
        (not (isOptionLine (idx + 1)) && not (isDescriptionOnly (idx + 1)))
          || (isDescriptionOnly (idx + 1) && (length (xs !! idx) + 5 > descriptionLineWidthTop10Percent))
          || isOptionLine (idx + 1) && offset >= 2 && last optSegment == ' ' && length (words descSegment) >= 2 -- [FIXME] too heuristic
          || isParsedAsOptDescLine && (length (xs !! idx) + 25 > descriptionLineWidthTop10Percent)
      where
        isOptionLine i = i `Set.member` optLineNumsSet
        isDescriptionOnly i = i `Set.member` descLineNumsWithoutOptionSet
        (optSegment, descSegment) = splitAt offset (xs !! idx)
        isParsedAsOptDescLine = not . null . parseLine $ (xs !! idx)

    descLineNumsWithOption =
      infoMsg
        "descLineNumsWithOption"
        [ idx | idx <- optLineNums, isWordStartingAround 2 offset (xs !! idx), isOptionAndDescriptionLine idx
        ]
    descLineNums = infoMsg "descLineNums" $ nubSort (descLineNumsWithoutOption ++ descLineNumsWithOption)

    (quartets, dropped) = toConsecutiveRangeQuartets optLineNums descLineNums
    quartetsMod = infoMsg "quartets" $ [(a, b, updateDescFrom xs offset a c, d) | (a, b, c, d) <- quartets] -- [(optFrom, optTo, descFrom, descTo)]
    res = concatMap (handleQuartet xs offset) quartetsMod

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

-- ================================================

squashOptionsAndDescriptionsNoOverlap :: [String] -> Int -> Int -> Int -> Int -> (String, String)
squashOptionsAndDescriptionsNoOverlap xs offset a b c = (opt, desc)
  where
    optLines = map (trim . (xs !!)) $ take (b - a) [a, a + 1 ..]
    opt = List.intercalate "," optLines
    descLines = map (drop offset . (xs !!)) $ take (c - b) [b, b + 1 ..]
    desc = smartUnwords descLines

squashOptionsAndDescriptionsOverlap :: [String] -> Int -> Int -> Int -> Int -> (String, String)
squashOptionsAndDescriptionsOverlap xs offset a b c = (opt, desc)
  where
    optLines = map (xs !!) $ take (b - a) [a, a + 1 ..]
    optLinesLastTruncated = map trim (init optLines ++ [take offset (last optLines)])
    opt = List.intercalate "," optLinesLastTruncated
    descLines = map (drop offset . (xs !!)) $ take (c - b + 1) [b - 1, b ..]
    desc = smartUnwords descLines

oneliners :: [String] -> Int -> Int -> Int -> [(String, String)]
oneliners xs offset a b =
  [ (trim former, latter)
    | i <- take (b - a) [a, a + 1 ..],
      let (former, latter) = splitAt offset (xs !! i)
  ]

updateDescFrom :: [String] -> Int -> Int -> Int -> Int
updateDescFrom xs offset optFrom descFrom
  | null ys = descFrom
  | otherwise = debugMsg "updateDescFrom (res) =" res
  where
    indices = take (optFrom - descFrom) [descFrom - 1, descFrom - 2 ..]
    ys = takeWhile (\i -> isWordStartingAround 2 offset (xs !! i)) indices
    res = last ys

-- | Returns (optFrom, optTo, descFrom, descTo) quartets AND dropped indices xs
toConsecutiveRangeQuartets :: [Int] -> [Int] -> ([(Int, Int, Int, Int)], [(Int, Int)])
toConsecutiveRangeQuartets xs ys =
  (res, dropped)
  where
    (xRanges, yRanges) = makeRanges xs ys
    res = mergeRangesFast xRanges yRanges
    xsRes = Set.fromList [(x1, x2) | (x1, x2, _, _) <- res]
    dropped = filter (`Set.notMember` xsRes) xRanges

-- | Make pairs of ranges such that
-- when two ranges (x1, x2) and (y1, y2) overlaps,
-- they always satisfy   x1 <= y1 <= x2 <= y2
-- As a special case, x2 == y1 is considered as "overlap"
-- although [x1, x2) and [y1, y2) have empty intersection.
makeRanges :: [Int] -> [Int] -> ([(Int, Int)], [(Int, Int)])
makeRanges xs ys =
  (xRanges, yRanges)
  where
    xStarts = map fst (toRanges xs)
    yEnds = map snd (toRanges ys)
    (xssHead, xss) = List.mapAccumR f xs yEnds
    f acc y = span (< y) acc
    xRanges = concatMap toRanges (xssHead : if null xss then [] else init xss)
    (_, yss) = List.mapAccumR g ys xStarts
    g acc x = span (< x) acc
    yRanges = concatMap toRanges yss

-- | [deprecated] O(N^2) so replaced with mergeRangesFast
mergeRanges :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int, Int)]
mergeRanges xs ys = [(x1, x2, y1, y2) | (x1, x2) <- xs, (y1, y2) <- ys, x1 <= y1 && y1 <= x2 && x2 <= y2]

-- | Create quartets (x1, x2, y1, y2) as overlapping boundaries
-- [Note] As a special case, x2 == y1 is considered as a overlap
-- although [x1, x2) and [y1, y2) have empty intersection.
mergeRangesFast :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int, Int)]
mergeRangesFast _ [] = []
mergeRangesFast [] _ = []
mergeRangesFast ((x1, x2) : xs) ((y1, y2) : ys)
  | x2 < y1 = mergeRangesFast xs ((y1, y2) : ys)
  | y2 <= x1 = mergeRangesFast ((x1, x2) : xs) ys
  | otherwise = assert cond $ (x1, x2, y1, y2) : mergeRangesFast xs ys
  where
    cond = x1 <= y1 && y1 <= x2 && x2 <= y2

-- |  idxRange idxColFrom (inclusive) lines
--  extractRectangleToRight (2, 5) 3
--  ........
--  ........
--  ...xxxxx
--  ...xxxxx
--  ...xxxxx
--  ........
extractRectangleToRight :: (Int, Int) -> Int -> [String] -> String
extractRectangleToRight (rowFrom, rowTo) idxCol xs =
  unwords zs
  where
    ys = take (rowTo - rowFrom) (drop rowFrom xs)
    zs = map (drop idxCol) ys

-- | Get line indices of headers
getHeadingIndices :: [String] -> [Int]
getHeadingIndices [] = []
getHeadingIndices xs
  | count >= 2 || null indentations' = [idx | (idx, indentation) <- zip [0 ..] indentations, indentation == minval]
  | otherwise = [idx | (idx, indentation) <- zip [0 ..] indentations, indentation == secondMinval]
  where
    indentations = debugMsg "indentations: " $ map (\x -> if null x then 80 else length . takeWhile (== ' ') $ x) xs
    minval = List.minimum indentations
    count = length $ filter (== minval) indentations
    indentations' = filter (/= minval) indentations
    secondMinval = List.minimum indentations'

splitByHeaders :: [String] -> [[String]]
splitByHeaders xs
  | any Utils.startsWithLongOption headings || any Utils.startsWithShortOrOldOption headings = [xs]
  | otherwise = map tail $ filter (\lines_ -> length lines_ > 1 && any Utils.startsWithDash lines_) $ Utils.splitsAt xs headingIndices
  where
    headingIndices = debugMsg "headingIndices: " (getHeadingIndices xs)
    headings = map (xs !!) headingIndices

preprocessBlockwise :: String -> [(String, String)]
preprocessBlockwise content = trace decoratedMsg $ concatMap preprocessAll contents
  where
    xs = lines content
    contents = map unlines $ splitByHeaders xs
    msg
      | null contents = "[warn] Found no block containing options!"
      | length contents == 1 = "[info] Found a single block with options"
      | otherwise = printf "[info] Block-wise processing (#blocks = %d)" (length contents)
    decoratedMsg = "\n-------------------------------------------\n" ++ msg ++ "\n-------------------------------------------\n"

parseBlockwise :: String -> [Opt]
parseBlockwise "" = []
parseBlockwise s = List.nub . concat $ results
  where
    pairs = preprocessBlockwise s
    results =
      [ (\xs -> if null xs then warnShow "Failed pair:" (optStr, descStr) xs else xs) $
          parseWithOptPart optStr descStr
        | (optStr, descStr) <- pairs,
          (optStr, descStr) /= ("", "")
      ]

preprocessAll :: String -> [(String, String)]
preprocessAll content = map (\(opt, desc) -> (trim opt, unwords $ words $ trim desc)) res
  where
    xs = lines content
    may = getOptionDescriptionPairsFromLayout content
    res = case may of
      Just (layoutResults, droppedIdxRanges) ->
        layoutResults ++ fallbackResults
        where
          paragraphs = map (getParagraph xs) droppedIdxRanges
          fallbackResults = infoMsg "opt-desc pairs from the fallback\n" $ concatMap preprocessAllFallback paragraphs
      Nothing ->
        trace
          "\n===============================================\n\
          \[warn] ignore layout: processing with fallback \n\
          \===============================================\n"
          $ preprocessAllFallback content

parseMany :: String -> [Opt]
parseMany "" = []
parseMany s = List.nub . concat $ results
  where
    pairs = preprocessAll s
    results =
      [ (\xs -> if null xs then warnShow "Failed pair:" (optStr, descStr) xs else xs) $
          parseWithOptPart optStr descStr
        | (optStr, descStr) <- pairs,
          (optStr, descStr) /= ("", "")
      ]
