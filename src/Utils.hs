{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | get statistical mode (= the most frequently appeareing item)
module Utils where

import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.Extra (nubSort, trimStart)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
import qualified Constants as Const

getMostFrequent :: (Ord a) => [a] -> Maybe a
getMostFrequent = fmap fst . getMostFrequentWithCount

count :: (Ord a) => [a] -> [(a, Int)]
count xs = Map.toList $ Map.fromListWith (+) (map (,1) xs)

getMostFrequentWithCount :: (Ord a) => [a] -> Maybe (a, Int)
getMostFrequentWithCount [] = Nothing
getMostFrequentWithCount xs = Just (x, maxCount)
  where
    counter = count xs
    (x, maxCount) = Foldable.maximumBy (compare `on` snd) counter

convertTabsToSpaces :: Int -> Text -> Text
convertTabsToSpaces n = T.unlines . map convertLine . T.lines . removeCrNewline
  where
    removeCrNewline = T.replace "\r\n" "\n"
    convertLine = List.foldl1' f . T.splitOn "\t"
    f acc t = T.concat [acc, T.replicate spaceWidth " ", t]
      where
        w = T.length acc
        offset = (w `div` n) * n + n
        spaceWidth = offset - w

removeDelimiter :: Char -> Text -> Text
removeDelimiter ch = T.intercalate "   " . T.splitOn from_
  where
    from_ = T.pack [' ', ch, ' ']

debugTag :: String
debugTag = "[debug]"

infoTag :: String
infoTag = "[info]"

warnTag :: String
warnTag = "[warn]"

traceMsgHelper :: (Show a) => String -> String -> a -> a
traceMsgHelper tag msg x = trace (unwords [tag, msg, show x, "\n"]) x

traceShowHelper :: (Show b) => String -> String -> b -> a -> a
traceShowHelper tag msg x = trace (unwords [tag, msg, show x ++ "\n"])

debugMsg :: (Show a) => String -> a -> a
debugMsg = traceMsgHelper debugTag

infoMsg :: (Show a) => String -> a -> a
infoMsg = traceMsgHelper infoTag

warnMsg :: (Show a) => String -> a -> a
warnMsg = traceMsgHelper warnTag

debugShow :: (Show b) => String -> b -> a -> a
debugShow = traceShowHelper debugTag

infoShow :: (Show b) => String -> b -> a -> a
infoShow = traceShowHelper infoTag

warnShow :: (Show b) => String -> b -> a -> a
warnShow = traceShowHelper warnTag

traceHelper :: String -> String -> a -> a
traceHelper tag msg = trace (unwords [tag, show msg])

debugTrace :: String -> a -> a
debugTrace = traceHelper debugTag

infoTrace :: String -> a -> a
infoTrace = traceHelper infoTag

warnTrace :: String -> a -> a
warnTrace = traceHelper warnTag

traceIf :: (a -> Bool) -> (a -> String) -> a -> a
traceIf check run x
  | check x = trace (run x) x
  | otherwise = x

-- | hyphen-aware unwords
-- Here supporting hypen as in unicode \8208 (decimal) = \2010 (hex)
-- https://unicode-table.com/en/2010/
--
-- [FIXME] `smartUnwords ["git-", "status"]` should be "git-status" instead of "gitstatus".
smartUnwords :: [String] -> String
smartUnwords =
  foldr1 f
  where
    hyphens = ['-', '\8208']
    f "" acc = acc
    f s "" = s
    f s acc
      | length s > 1 && c `elem` hyphens && c2 `List.notElem` (' ' : hyphens) = initS ++ acc
      | otherwise = s ++ (' ' : acc)
      where
        c = last s
        initS = init s
        c2 = last initS

-- | convert strictly-increasing ints to a list of left-inclusive right-exclusive ranges
-- toRanges [1,2,3,4,6,9,10] == [(1, 5), (6, 7), (9, 11)]
-- assert the input is sorted
toRanges :: [Int] -> [(Int, Int)]
toRanges = foldr f []
  where
    f x [] = [(x, x + 1)]
    f x ((a, b) : rest)
      | x + 1 == a = (x, b) : rest
      | otherwise = (x, x + 1) : (a, b) : rest

-- | convert from left-inclusive right-exclusive ranges to a list of integers
fromRanges :: [(Int, Int)] -> [Int]
fromRanges = nubSort . concatMap fromRange

fromRange :: (Int, Int) -> [Int]
fromRange (a, b) = [a, (a + 1) .. (b - 1)]

contains :: [(Int, Int)] -> Int -> Bool
contains ranges x = any (\(a, b) -> a <= x && x < b) ranges

getParagraph :: [String] -> (Int, Int) -> String
getParagraph xs range = unlines $ map (xs !!) (fromRange range)

-- | check if the string starts with non-space char `c`
startsWithChar :: Char -> String -> Bool
startsWithChar c s = (not . null) ss && head ss == c
  where
    ss = trimStart s

-- | check if the string starts with dash - possibly after spaces and tabs
startsWithDash :: String -> Bool
startsWithDash = startsWithChar '-'

-- | check if the string starts with -- possibly after spaces and tabs
startsWithDoubleDash :: String -> Bool
startsWithDoubleDash s = case ss of
  "" -> False
  [_] -> False
  c1 : c2 : _ -> c1 == '-' && c2 == '-'
  where
    ss = trimStart s

startsWithSingleDash :: String -> Bool
startsWithSingleDash s = case ss of
  "" -> False
  [_] -> False
  c1 : c2 : _ -> c1 == '-' && c2 /= '-'
  where
    ss = trimStart s

startsWithLongOption :: String -> Bool
startsWithLongOption s = startsWithDoubleDash s && length ss >= 3 && c `notElem` [' ', '-']
  where
    ss = trimStart s
    _ : _ : c : _ = ss

startsWithShortOrOldOption :: String -> Bool
startsWithShortOrOldOption s = startsWithDash s && length ss >= 2 && c `notElem` [' ', '-']
  where
    ss = trimStart s
    _ : c : _ = ss

-- | A speculative criteria for non-critical purposes
mayContainOptions :: [Text] -> Bool
mayContainOptions = (>= 2) . length . filter (T.isPrefixOf "-" . T.stripStart)

-- | Another speculative criteria for non-critical purposes
mayContainSubcommands :: [Text] -> Bool
mayContainSubcommands = (>= 4) . length . filter ((>= 2) . length . T.words) . filter (\t -> " " `T.isPrefixOf` t || "\t" `T.isPrefixOf` t) . filter (not . T.null)

getTopLevelHeadingIndices :: [Text] -> [Int]
getTopLevelHeadingIndices xs
  | null xs = []
  | otherwise = [idx | (idx, indentation) <- zip [0 ..] indentations, indentation == minval]
  where
    indentations = map (\x -> if T.null x then 80 else (T.length . T.takeWhile (== ' ')) x) xs
    minval = List.minimum indentations

splitByTopHeaders :: Text -> [Text]
splitByTopHeaders text = map T.unlines $ splitsAt xs headingIndices
  where
    xs = T.lines text
    headingIndices = getTopLevelHeadingIndices xs

dropUsage :: Text -> Text
dropUsage text = res
  where
    xs = splitByTopHeaders text
    rest = filter (not . isUsageBlock) xs
    res =
      if null rest
        then text
        else T.concat rest

isUsageBlock :: Text -> Bool
isUsageBlock = (\s -> ("usage" `T.isPrefixOf` s) || ("synopsis" `T.isPrefixOf` s)) . T.toLower . T.stripStart

-- | A speculative criteria for non-critical purposes
-- [TODO] Scrutinize this as it's now used for critical purposes
hasErrorMessageAtTop :: Text -> Text -> Bool
hasErrorMessageAtTop name text
  | (T.null . T.stripStart) text = False
  | otherwise = any (`T.isInfixOf` loweredFirstLine) errKeywords
  where
    --
    errKeywords = filter (\k -> not (k `T.isInfixOf` name)) Const.errKeywords
    loweredFirstLine = (T.toLower . T.take 100 . head . T.lines . T.stripStart) text


mayContainUseful :: Text -> Text -> Bool
mayContainUseful name text  = length xs >= 2
  where
    xs = filter (isNotNullAndErrorMessageAbsent name) . T.lines $ text

-- | Check if a text is free from error-like words
isNotNullAndErrorMessageAbsent :: Text -> Text -> Bool
isNotNullAndErrorMessageAbsent name text =
  (not . T.null . T.strip) lowered && not (any (`T.isInfixOf` lowered) errKeywords)
  where
    errKeywords = filter (\k -> not (k `T.isInfixOf` name)) Const.errKeywords
    lowered = (T.toLower . T.strip) text

-- | splitsAt ... like Data.List.splitAt but multiple indices
--
-- >>> splitsAt [0, 2, 4, 6, 8, 10] [0, 3, 5]
-- [[0, 2, 4], [6, 8], [10]]
splitsAt :: [a] -> [Int] -> [[a]]
splitsAt xs ns = reverse $ filter (not . null) $ List.unfoldr f (xs, reverse ns')
  where
    ns' = List.sort ns
    f :: ([a], [Int]) -> Maybe ([a], ([a], [Int]))
    f ([], _) = Nothing
    f (ys, []) = Just (ys, ([], []))
    f (ys, k : ks) = Just (latter, (former, ks))
      where
        (former, latter) = List.splitAt k ys

topTenPercentile :: (Ord a) => [a] -> a
topTenPercentile [] = error "Cannot compute percentile against null!"
topTenPercentile xs = sortedXs !! idx
  where
    n = length xs
    idx = fromInteger $ floor (fromIntegral (n - 1) * 0.9 :: Rational) :: Int
    sortedXs = List.sort xs

isBracketed :: Text -> Bool
isBracketed text
  | T.length text <= 1 = False
  | otherwise = (T.head text, T.last text) `elem` bracketPairs

hasClosedBrackets :: Char -> Char -> Text -> Bool
hasClosedBrackets bra ket = helper 0
  where
    helper :: Int -> Text -> Bool
    helper acc txt
      | T.null txt = acc == 0
      | acc < 0 = False
      | otherwise = helper (acc + diff) rest
      where
        c = T.head txt
        rest = T.tail txt
        diff
          | c == bra = 1
          | c == ket = -1
          | otherwise = 0

hasMatchingBrackets :: Text -> Bool
hasMatchingBrackets text = hasBra && allCleared
  where
    (bras, _) = unzip bracketPairs
    hasBra = any (\b -> T.singleton b `T.isInfixOf` text) bras
    allCleared = and [hasClosedBrackets bra ket text | (bra, ket) <- bracketPairs]

bracketPairs :: [(Char, Char)]
bracketPairs =
  [ ('{', '}'),
    ('<', '>'),
    ('[', ']'),
    ('(', ')')
  ]

-- | Split increasing integers into contiguous chunks
--
-- >>> toContiguousChunks [2, 3, 4, 8, 10, 11]
-- [[2, 3, 4], [8], [10, 11]]
toContiguousChunks :: [Int] -> [[Int]]
toContiguousChunks = List.unfoldr f
  where
    f :: [Int] -> Maybe ([Int], [Int])
    f [] = Nothing
    f xs = Just $ splitAt (n + 1) xs
      where
        n = length $ takeWhile (== 1) $ map (\(x, xNext) -> xNext - x) $ zip xs (tail xs)

toFstContiguousChunks :: [(Int, a)] -> [[(Int, a)]]
toFstContiguousChunks = List.unfoldr f
  where
    f :: [(Int, a)] -> Maybe ([(Int, a)], [(Int, a)])
    f [] = Nothing
    f xs = Just $ splitAt (n + 1) xs
      where
        n = length $ takeWhile (== 1) $ map (\(x, xNext) -> fst xNext - fst x) $ zip xs (tail xs)
