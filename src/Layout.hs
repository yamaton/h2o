{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Layout where

import Control.Exception (assert)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import Data.List.Extra (breakOnEnd, nubSort, trim, trimEnd)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (trace)
import qualified HelpParser
import Text.Printf (printf)
import Type (Opt)
import Utils (debugMsg, getMostFrequent, getMostFrequentWithCount, infoMsg, infoShow, warnShow)
import qualified Utils

-- | Location is defined by (row, col) order
type Location = (Int, Int)

-- [TODO] memoise the calls
-- https://stackoverflow.com/questions/3208258/memoization-in-haskell

-- | Get location right before '-' in the head of a line
--
-- λ> getOptionLocations " \n\n          --option here\n  --baba"
-- [(2, 10), (3, 2)]
getOptionLocations :: String -> [Location]
getOptionLocations = _getNonblankLocationTemplate Utils.startsWithDash

-- | Get locations of lines NOT starting with dash
--
-- λ> getNonoptLocations " \n\n          --option here\n  --baba"
-- [(0, 1), (1, 0)]
getNonoptLocations :: String -> [Location]
getNonoptLocations = _getNonblankLocationTemplate (not . Utils.startsWithDash)

-- | a helper function
_getNonblankLocationTemplate :: (String -> Bool) -> String -> [Location]
_getNonblankLocationTemplate f s =
  [(i, getHorizOffset x) | (i, x) <- enumLines, (not . null . trim) x, f x]
  where
    enumLines = zip [(0 :: Int) ..] (lines s)
    getHorizOffset = length . takeWhile (== ' ')

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

-- | get estimated horizontal offset of long options
getLongOptionOffset :: String -> Maybe Int
getLongOptionOffset = _getOffsetHelper getLongOptionLocations

-- | get estimated horizontal offset of long options
getShortOptionOffset :: String -> Maybe Int
getShortOptionOffset = _getOffsetHelper getShortOptionLocations

-- | helper: get a frequency-based estimate of horizontal offset
_getOffsetHelper :: (String -> [Location]) -> String -> Maybe Int
_getOffsetHelper getLocs s = traceMessage res
  where
    locs = getLocs s
    offsets = map snd locs
    res = getMostFrequent offsets
    droppedOptionLinesInfo = unlines [(printf "layout: dropped lines: (%03d) %s" r (lines s !! r) :: String) | (r, c) <- locs, Just c /= res]
    traceMessage = Utils.infoTrace droppedOptionLinesInfo

-- | Returns the estimate of description offset after looking at all lines
-- AND option locations that failed to satisfy the layout.
-- There are two independent ways to guess the horizontal offset of descriptions:
--   1) A description line may be simply indented by space
--   2) A description line may appear following an option
--      	... from the pattern that the description and the options+args
--      	may be separated by 3 or more spaces
-- Returns Nothing if 1 and 2 disagrees, or no information in 1 and 2
getDescOffsetEstimate :: Int -> String -> [Location] -> (Maybe Int, [Location])
getDescOffsetEstimate lineIdxBase s optLocs =
  case descOffsetWithCountInNonoptLines lineIdxBase s optLocs of
    (Nothing, optLocsRemoved) ->
      case descOffsetWithCountInOptionLines lineIdxBase s (filter (`notElem` optLocsRemoved) optLocs) of
        Nothing -> Utils.infoTrace "Retrieved absolutely zero information" (Nothing, optLocsRemoved)
        Just (x2, c2) ->
          if isAlignedMoreThan75Percent c2
            then Utils.infoTrace "Descriptions always appear in the lines with options" (Just x2, optLocsRemoved)
            else Utils.infoTrace "Retrieved nothing from layout" (Nothing, optLocsRemoved)
    (Just (x1, c1), optLocsRemoved) ->
      case descOffsetWithCountInOptionLines lineIdxBase s (filter (`notElem` optLocsRemoved) optLocs) of
        Nothing -> Utils.infoTrace "Descriptions never appear in the lines with options" (Just x1, optLocsRemoved)
        Just (x2, c2)
          | x1 == x2 -> (Just x1, optLocsRemoved)
          | c1 <= 3 && 3 < c2 && isAlignedMoreThan75Percent c2 -> (debug Just x2, optLocsRemoved)
          | c2 <= 3 && 3 < c1 -> (debug Just x1, optLocsRemoved)
          | 0 < x1 - x2 && x1 - x2 < 5 && isAlignedMoreThan75Percent c2 -> (debug (Just x2), optLocsRemoved) -- sometimes continued lines are indented.
          | otherwise -> (debug Nothing, optLocsRemoved)
          where
            msg =
              "Disagreement in offsets:\n\
              \   description-only-line offset   %d (with count %d)\n\
              \   option+description-line offset %d (with count %d)\n"
            debug = Utils.warnTrace (printf msg x1 c1 x2 c2 :: String)
  where
    isAlignedMoreThan75Percent c = c * 100 >= 75 * length optLocs

-- | Estimate offset of description in non-option lines.
--   Returns (Just (description offset, match count), [removed option locations]) if matches
descOffsetWithCountInNonoptLines :: Int -> String -> [Location] -> (Maybe (Int, Int), [Location])
descOffsetWithCountInNonoptLines lineIdxBase s optLocs
  | null offsetOverlaps = (res, [])
  | otherwise = (res, optLocsRemoved)
  where
    descLocs =
      infoMsg (printf "descLocs (line+%d): lineIdxBase" lineIdxBase) $
        takeHangingDesc lineIdxBase optLocs $ getNonoptLocations s
    (_, descOffsets) = unzip descLocs
    (optLineNums, optOffsets) = unzip optLocs
    offsetOverlaps = Set.toList $ Set.intersection (Set.fromList optOffsets) (Set.fromList descOffsets)
    (optOffsets', optOffsetsRemoved) = span (< 10) optOffsets
    optLocsRemoved =
      infoMsg (printf "optLocsRemoved (line+%d): " lineIdxBase) $
        filter (\(_, c) -> c `elem` optOffsetsRemoved) optLocs
    indentations =
      infoMsg "descIndentations:" $
        [ x | (r, x) <- descLocs,
              -- description's offset is equal (rare case!) or greater than option's
              null optOffsets' || (List.maximum optOffsets' <= x),
              -- description can exist only around option lines
              (not . null) optLineNums, head optLineNums < r, r < last optLineNums + 5
              -- previous line cannot be blank
        ]
    res = infoMsg "descOffsetWithCountInNonoptLines: " $ getMostFrequentWithCount indentations

-- | Take description locations that hangs an option line
--
--  Consider follwing empty-line delimited patterns:
--
--    Heading                                                       <--- NOT hanging
--      --option arg   description
--                     continued description                        <--- hanging
--
--    Another heading                                               <--- NOT hanging
--      --option arg
--           description                                            <--- hanging
--
--      --option (arg) Somehow explanation immediately follows
--      and it's continued to the next lines without indentation.   <--- hanging
--
--         --option arg
--      When following description lines are indented less than     <--- NOT hanging
--      the option line itself, they are not hanging.               <--- NOT hanging
--
--      Some descriptions are not tied to particular options, yet show    <--- NOT hanging
--      --option in the middle of the sentences. This case should be
--      excluded from description statistics.                             <--- NOT hanging
--
--    Something blah                           <--- NOT hanging
--      more blah...                           <--- NOT hanging
takeHangingDesc :: Int -> [Location] -> [Location] -> [Location]
takeHangingDesc lineIdxBase optLocs descLocs = descLocSelected
  where
    (optLineNums, _) = unzip optLocs
    (descLineNums, _) = unzip descLocs
    cueDescLocs =
      infoMsg (printf "cueDescLocs (line+%d):" lineIdxBase) $
        [ (descLineNum, descIndentation)
          | (i, (descLineNum, descIndentation)) <- zip [0 ..] descLocs,
            (descLineNum - 1) `elem` optLineNums,
            let xs = [c | (r, c) <- optLocs, r == (descLineNum - 1)],
            (not . null) xs,
            let optIndentation = head xs,
            descIndentation >= optIndentation,
            descIndentation > optIndentation
              || (descLineNum - 2) `notElem` descLineNums
              || i == 0
              || snd (descLocs !! (i - 1)) /= optIndentation
        ]
    (cueLineNums, _) = unzip cueDescLocs
    descLocChunks = Utils.toFstContiguousChunks descLocs
    descLocChunks' = filter (\chunk -> (not . null) chunk && fst (head chunk) `elem` cueLineNums) descLocChunks
    descLocSelected =
      concatMap
        (\chunk -> takeWhile (\(_, c) -> (not . null) chunk && c == snd (head chunk)) chunk)
        descLocChunks'

-- | Get empty line numbers.
-- Here an empty line inclues line filled with whitespaces.
getEmptyLineNums :: String -> [Int]
getEmptyLineNums s = res
  where
    xs = lines s
    numLinePairs = zip [0 ..] xs
    (res, _) = unzip $ filter (\(_, x) -> null . trim $ x) numLinePairs

-- | Estimate offset of description part from the lines with options.
-- Returns Just (offset size, match count) if matches
descOffsetWithCountInOptionLines :: Int -> String -> [Location] -> Maybe (Int, Int)
descOffsetWithCountInOptionLines _ s optLocs =
  infoMsg "descOffsetWithCountInOptionLines: " res
  where
    sep = "   " -- hardcoded as 3 spaces for now
    xs = lines s
    optLines = map ((xs !!) . fst) optLocs
    xss = map (fst . breakOnEnd sep . trimEnd) optLines
    res =
      getMostFrequentWithCount $
        map length $
          filter (not . null . trim) xss

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
    condAfter = (not . null) after && head after /= ' '

-- | Check if a word starting at the horizontal position.
-- [NOTE] Assume word delimiter is a whitespace.
isWordStartingAtOffset :: Int -> String -> Bool
isWordStartingAtOffset _ "" = False
isWordStartingAtOffset 0 (c : _) = c /= ' '
isWordStartingAtOffset n x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    (not . null) before && (not . null) after && last before == ' ' && head after /= ' '
  where
    (before, after) = splitAt n x

-- | Check if a word starting around the horizontal position.
-- Ambiguity is set by margin value.
isWordStartingAround :: Int -> Int -> String -> Bool
isWordStartingAround _ _ "" = False
isWordStartingAround margin idx x =
  assert ('\n' `notElem` x && '\t' `notElem` x) $
    any criteria indices
  where
    indices = [idx - margin, idx - margin + 1 .. idx + margin]
    criteria i =
      (not . null) before && (not . null) after && last before == ' ' && head after /= ' '
      where
        (before, after) = splitAt i x

-- ================================================
-- ============== Main stuff ======================

-- | Returns option line's (1) consensus beginning of description
-- and (2) option locations [(row, col)]. All in 0-based indexing.
--
-- [FIXME] Should attempt more when descriptionOffsetMay is Nothing.
getDescOffsetOptLocsPair :: Int -> String -> (Maybe Int, [(Int, Int)])
getDescOffsetOptLocsPair lineIdxBase s
  | null optionOffsets = Utils.infoTrace "optionOffsets is null" (Nothing, [])
  | null optLocsFixed = Utils.infoTrace "optLocsFixed is null" (Nothing, [])
  | Maybe.isNothing descriptionOffsetMay = Utils.infoTrace "getDescOffsetOptLocsPair: descriptionOffsetMay is Nothing" (Nothing, optLocsFixed)
  | descOffset <= 3 = Utils.infoTrace "getDescOffsetOptLocsPair: descOffset too small" (Nothing, optLocsFixed)
  | otherwise = Utils.infoMsg "getDescOffsetOptLocsPair: " (Just descOffset, optLocsFixed)
  where
    optionOffsets = infoMsg "layout:optionOffsets:" $ getOptionOffsets s
    optLocsCandidates = getOptionLocations s

    -- Split the option locations by comparing the horizontal offsets with the most frequent one
    (optLocs, _) = List.partition (\(_, c) -> c `elem` optionOffsets) optLocsCandidates
    (descriptionOffsetMay, optLocsRemoved) =
      getDescOffsetEstimate lineIdxBase s $
        infoMsg (printf "optLocs (line+%d)" lineIdxBase) optLocs
    descOffset = infoMsg "layout:descOffset:" $ Maybe.fromJust descriptionOffsetMay
    optLocsFixed =
      infoMsg (printf "layout:optLineNumsFixed: (line+%d)" lineIdxBase) $
        filter (`notElem` optLocsRemoved) optLocs

getDescriptionOffset :: String -> Maybe Int
getDescriptionOffset s = fst $ getDescOffsetOptLocsPair 0 s

-- | Returns option-description pairs based on layouts
-- AND the option locations uncaught in the process.
getOptionDescriptionPairsFromLayout :: Int -> String -> Maybe ([(String, String)], [(Int, Int)])
getOptionDescriptionPairsFromLayout lineIdxBase s
  | Maybe.isNothing descOffsetMay || null res = Nothing
  | otherwise = Just $ infoShow (printf "droppedOptLocs (line+%d): " lineIdxBase) droppedOptLocs (res, droppedOptLocs)
  where
    (descOffsetMay, optLocs) = getDescOffsetOptLocsPair lineIdxBase s
    descOffset = Maybe.fromJust descOffsetMay
    xs = lines s
    optLineNums = map fst optLocs
    optLineNumsSet = Set.fromList optLineNums
    -- More accomodating description line matching seems to work better...
    descLineNumsWithoutOption =
      debugMsg
        (printf "descLineNumsWithoutOption (line+%d)" lineIdxBase)
        [ idx | (idx, x) <- zip [0 ..] xs, isWordStartingWithIndentation descOffset x, idx `Set.notMember` optLineNumsSet
        ]
    linewidths = map (length . (xs !!)) descLineNumsWithoutOption
    descLineWidthTop10Percentile = infoMsg "descLineWidthTop10Percentile: " $ if null linewidths then 80 else Utils.topTenPercentile linewidths
    descLineNumsWithoutOptionSet = Set.fromList descLineNumsWithoutOption

    -- The line must be long when description starts at the same line
    -- the option and continues to the next line.
    isOptionAndDescriptionLine idx
      | not (isOptionLine idx) = False
      | otherwise =
        (not (isOptionLine (idx + 1)) && not (isDescriptionOnly (idx + 1)))
          || (isDescriptionOnly (idx + 1) && (length (xs !! idx) + 6 > descLineWidthTop10Percentile))
          || isOptionLine (idx + 1) && descOffset >= 2 && last optSegment == ' ' && length (words descSegment) >= 2 -- [FIXME] too heuristic
          || isParsedAsOptDescLine && (length (xs !! idx) + 25 > descLineWidthTop10Percentile)
      where
        isOptionLine i = i `Set.member` optLineNumsSet
        isDescriptionOnly i = i `Set.member` descLineNumsWithoutOptionSet
        (optSegment, descSegment) = splitAt descOffset (xs !! idx)
        isParsedAsOptDescLine = not . null . HelpParser.parseLine $ (xs !! idx)

    descLineNumsWithOption =
      infoMsg
        (printf "descLineNumsWithOption (line+%d)" lineIdxBase)
        [ idx | idx <- optLineNums, isWordStartingAround 2 descOffset (xs !! idx), isOptionAndDescriptionLine idx
        ]
    descLineNums =
      infoMsg (printf "descLineNums (line+%d)" lineIdxBase) $
        nubSort (descLineNumsWithoutOption ++ descLineNumsWithOption)

    (quartets, droppedOptLineNums) = toConsecutiveRangeQuartets optLineNums descLineNums
    droppedOptLocs = filter (\(x, _) -> x `elem` droppedOptLineNums) optLocs
    quartetsMod = infoMsg "quartets" $ [(a, b, updateDescFrom xs descOffset a c, d) | (a, b, c, d) <- quartets] -- [(optFrom, optTo, descFrom, descTo)]
    res = concatMap (handleQuartet xs descOffset) quartetsMod

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
    desc = unlines descLines

squashOptionsAndDescriptionsOverlap :: [String] -> Int -> Int -> Int -> Int -> (String, String)
squashOptionsAndDescriptionsOverlap xs offset a b c = (opt, desc)
  where
    optLines = map (xs !!) $ take (b - a) [a, a + 1 ..]
    optLinesLastTruncated = map trim (init optLines ++ [take offset (last optLines)])
    opt = List.intercalate "," optLinesLastTruncated
    descLines = map (drop offset . (xs !!)) $ take (c - b + 1) [b - 1, b ..]
    desc = unlines descLines

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

-- | Returns (optFrom, optTo, descFrom, descTo) quartets
-- AND the dropped line indices in xs
toConsecutiveRangeQuartets :: [Int] -> [Int] -> ([(Int, Int, Int, Int)], [Int])
toConsecutiveRangeQuartets xs ys =
  (res, droppedOptLineNums)
  where
    (xRanges, yRanges) = makeRanges xs ys
    res = mergeRangesFast xRanges yRanges
    resXRanges = [(x1, x2) | (x1, x2, _, _) <- res]
    droppedOptLineNums = filter (not . Utils.contains resXRanges) xs

-- | Make pairs of overlapping ranges.
--
-- When two ranges (x1, x2) and (y1, y2) overlap,
-- they must satisfy x1 <= y1 <= x2 <= y2.
-- When x2 == y1, its still treated as "overlap"
-- although [x1, x2) and [y1, y2) have empty intersection.
--
-- [NOTE] this can drop some items in xs (after `last yEnds)
makeRanges :: [Int] -> [Int] -> ([(Int, Int)], [(Int, Int)])
makeRanges xs ys =
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

-- | [deprecated] O(N^2) so replaced with mergeRangesFast
mergeRanges :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int, Int)]
mergeRanges xs ys = [(x1, x2, y1, y2) | (x1, x2) <- xs, (y1, y2) <- ys, x1 <= y1 && y1 <= x2 && x2 <= y2]

-- | Create quartets (x1, x2, y1, y2) as overlapping boundaries
--
-- [Note] As a special case, x2 == y1 is considered as a overlap
-- although [x1, x2) and [y1, y2) have empty intersection.treatedd
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

-- | Split text by top-level headers
-- where headers are recognized by the least indentations
-- NOTE: the top-level headers are **included** in the output
--       this is not exclude headings starting with "- Hey this is heading!"
splitByHeaders :: [String] -> [(Int, String)]
splitByHeaders xs
  | any Utils.startsWithLongOption headings = [(0, unlines xs)]
  | any Utils.startsWithShortOrOldOption headings = [(0, unlines xs)]
  | otherwise = chunks
  where
    sepIndices = getHeadingIndices xs -- separater indices
    blockIndicesRaw =
      if null sepIndices || 0 `notElem` sepIndices
        then 0 : sepIndices
        else sepIndices
    headings = map (xs !!) blockIndicesRaw
    chunks =
      map (Bifunctor.second unlines) $
        filter (\(_, lines_) -> length lines_ > 1 && any Utils.startsWithDash lines_) $
          zip blockIndicesRaw $ Utils.splitsAt xs blockIndicesRaw

-- | Parse (option-and-argument, description) pairs from text by applying
-- preprocessAll to each header-based block.
preprocessBlockwise :: String -> [(String, String)]
preprocessBlockwise content = Utils.infoTrace decoratedMsg $ concatMap (uncurry preprocessAll) indexBlockWoHeaderPairs
  where
    xs = lines content
    indexBlockPairs = splitByHeaders xs
    indexBlockPairsWoUsage = filter (not . Utils.isUsageBlock . T.pack . snd) indexBlockPairs
    -- fix indices to compensate header-less content
    indexBlockWoHeaderPairs = map (\(i, s) -> (i + 1, tail s)) indexBlockPairsWoUsage
    msg = printf "Found %d header-based blocks" (length indexBlockWoHeaderPairs)
    decoratedMsg = "-------- " ++ msg ++ " --------"

-- | Parse options from text
parseBlockwise :: String -> [Opt]
parseBlockwise "" = []
parseBlockwise s = List.nub . concat $ results
  where
    pairs = preprocessBlockwise s
    results =
      [ (\xs -> if null xs then warnShow "Failed pair:" (optStr, descStr) xs else xs) $
          HelpParser.parseWithOptPart optStr descStr
        | (optStr, descStr) <- pairs,
          (optStr, descStr) /= ("", "")
      ]

preprocessMeta :: (Int -> String -> [(String, String)]) -> Int -> String -> [(String, String)]
preprocessMeta fallbackFunc lineIdxBase content = filter (/= ("", "")) $ map (Bifunctor.bimap trim cleanDescription) res
  where
    xs = lines content
    may = getOptionDescriptionPairsFromLayout lineIdxBase content
    res = case may of
      Just (layoutResults, droppedOptLocs) ->
        layoutResults ++ fallbackResults
        where
          descLineNumsExtra = map fst $ takeHangingDesc lineIdxBase droppedOptLocs $ getNonoptLocations content
          droppedOptLineNums = map fst droppedOptLocs
          lineNums = List.sort $ droppedOptLineNums ++ descLineNumsExtra
          rangeForFallback = infoMsg (printf "rangeForFallback (line+%d)" lineIdxBase) $ Utils.toRanges lineNums
          paragraphs = map (Utils.getParagraph xs) rangeForFallback
          indices = map fst rangeForFallback
          fallbackResults = infoMsg "opt-desc pairs from the fallback\n" $ concatMap (uncurry fallbackFunc) (zip indices paragraphs)
      Nothing ->
        trace
          "\n===============================================\n\
          \[warn] ignore layout: processing with fallback \n\
          \===============================================\n"
          $ HelpParser.preprocessAllFallback content
    cleanDescription = unwords . words . Utils.smartUnwords . lines

-- |  Parse (option-and-argument, description) pairs from text
preprocessAll :: Int -> String -> [(String, String)]
preprocessAll = preprocessMeta preprocessSecondAttempt

preprocessSecondAttempt :: Int -> String -> [(String, String)]
preprocessSecondAttempt = preprocessMeta (\_ s -> HelpParser.preprocessAllFallback s)

-- | Deprecated. Parse options without header-based splitting of input text
parseMany :: String -> [Opt]
parseMany "" = []
parseMany s = List.nub . concat $ results
  where
    pairs = preprocessAll 0 s
    results =
      [ (\xs -> if null xs then warnShow "Failed pair:" (optStr, descStr) xs else xs) $
          HelpParser.parseWithOptPart optStr descStr
        | (optStr, descStr) <- pairs,
          (optStr, descStr) /= ("", "")
      ]
