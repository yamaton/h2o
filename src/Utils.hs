{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | get statistical mode (= the most frequently appeareing item)
module Utils where

import Config (isVerbose)
import qualified Constants as Const
import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.Extra (nubSort, trimStart)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as Char
import qualified Debug.Trace as Debug

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

unicodeSpacesToAscii :: Text -> Text
unicodeSpacesToAscii = T.replace "\x00a0" " "

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

trace :: String -> a -> a
trace msg x = if isVerbose then Debug.trace msg x else x

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

-- | hyphen-aware `unwords` meant for hyphenated `lines`
--
-- Supports hypens as in unicode \8208 (decimal) = \2010 (hex)
-- https://unicode-table.com/en/2010/
--
-- [NOTE] `smartUnwords ["git-", "status"]` produces "gitstatus":
-- as it handles soft hyphen logic, NOT hard hyphen logic
smartUnwords :: [String] -> String
smartUnwords [] = ""
smartUnwords xs =
  foldr1 f xs
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
startsWithChar c s =
  case trimStart s of
    (x : _) -> x == c
    [] -> False

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
    c = case ss of
      (_ : _ : c' : _ : _) | length ss >= 3 -> c'
      _ -> '\0' -- never executed and won't affect the result

startsWithShortOrOldOption :: String -> Bool
startsWithShortOrOldOption s = startsWithDash s && length ss >= 2 && c `notElem` [' ', '-']
  where
    ss = trimStart s
    c = case ss of
      (_ : c' : _) | length ss >= 2 -> c'
      _ -> '\0' -- never executed and won't affect the result

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
hasErrorMessageAtTop name text =
  case T.lines (T.stripStart text) of
    [] -> False
    (l : _) ->
      let loweredFirstLine = T.toLower . T.take 100 $ l
          validKeywords = filter (not . (`T.isInfixOf` name)) Const.errKeywords
       in not (Utils.isUsageBlock loweredFirstLine)
            && any (`T.isInfixOf` loweredFirstLine) validKeywords

mayContainUseful :: Text -> Text -> Bool
mayContainUseful name text
  | null xs = False
  | length xs == 1 = "usage" `T.isPrefixOf` (T.toLower . T.stripStart . head) xs
  | name == "gatk" = length xs >= 4 -- special handling for GATK
  | otherwise = True
  where
    xs = filter (isNotNullAndErrorMessageAbsent name) . T.lines $ text

-- | Check if a text is free from error-like words at the bottom of the page.
isNotNullAndErrorMessageAbsent :: Text -> Text -> Bool
isNotNullAndErrorMessageAbsent name text =
  isNotNull && (isUsageBlock bottomLine || isBottomWithoutError)
  where
    errKeywords = filter (\k -> not (k `T.isInfixOf` name)) Const.errKeywords
    lowered = (T.toLower . T.strip) text
    isNotNull = (not . T.null) lowered
    bottomLine = last (T.lines lowered)
    isBottomWithoutError = not (any (`T.isInfixOf` bottomLine) errKeywords)

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
topTenPercentile [] = error "topTenPercentile: empty list (this is a bug in h2o)"
topTenPercentile xs = sortedXs !! idx
  where
    n = length xs
    idx = fromInteger $ floor (fromIntegral (n - 1) * 0.9 :: Rational) :: Int
    sortedXs = List.sort xs

-- | Checks if the text is wrapped in matching brackets defined in 'bracketPairs'.
-- Complexity: O(1)
isWrappedInBrackets :: Text -> Bool
isWrappedInBrackets txt = case T.uncons txt of
  Just (h, rest) -> case T.unsnoc rest of
    Just (_, l) -> (h, l) `elem` bracketPairs
    Nothing -> False -- Text has only 1 character
  Nothing -> False -- Text is empty

-- | Checks if the given 'Text' contains balanced brackets.
-- Complexity: O(n) time, O(1) space.
hasBalancedBrackets :: Char -> Char -> Text -> Bool
hasBalancedBrackets bra ket = go 0
  where
    go :: Int -> Text -> Bool
    go !acc txt = case T.uncons txt of
      Nothing -> acc == 0
      Just (c, rest)
        | acc < 0 -> False -- Short-circuit early
        | c == bra -> go (acc + 1) rest
        | c == ket -> go (acc - 1) rest
        | otherwise -> go acc rest

hasMatchingBrackets :: Text -> Bool
hasMatchingBrackets text = hasBra && allCleared
  where
    (bras, _) = unzip bracketPairs
    hasBra = any (\b -> T.singleton b `T.isInfixOf` text) bras
    allCleared = and [hasBalancedBrackets bra ket text | (bra, ket) <- bracketPairs]

bracketPairs :: [(Char, Char)]
bracketPairs =
  [ ('{', '}'),
    ('<', '>'),
    ('[', ']'),
    ('(', ')')
  ]

-- | Group elements into chunks where the projected integer key is consecutive.
groupConsecutiveOn :: (a -> Int) -> [a] -> [[a]]
groupConsecutiveOn f = foldr step []
  where
    step x [] = [[x]]
    step x (g@(y : _) : gs)
      | f y == f x + 1 = (x : g) : gs
      | otherwise = [x] : g : gs

-- | Specialized for Int
groupConsecutive :: [Int] -> [[Int]]
groupConsecutive = groupConsecutiveOn id

-- | Specialized for Tuples
groupConsecutiveByFst :: [(Int, a)] -> [[(Int, a)]]
groupConsecutiveByFst = groupConsecutiveOn fst

addOffsetToLines :: Int -> [(Int, Int)] -> [(Int, Int)]
addOffsetToLines offset pairs = [(line + offset, character) | (line, character) <- pairs]

infoShowIndices :: String -> Int -> [Int] -> [Int]
infoShowIndices s offset indices = infoShow s coordsModified indices
  where
    coordsModified = map (+ offset) indices

infoShowCoords :: String -> Int -> [(Int, Int)] -> [(Int, Int)]
infoShowCoords s offset coords = infoShow s coordsModified coords
  where
    coordsModified = addOffsetToLines offset coords

infoShowQuartets :: String -> Int -> [(Int, Int, Int, Int)] -> [(Int, Int, Int, Int)]
infoShowQuartets s offset quartets = infoShow s modified quartets
  where
    modified = [(a + offset, b + offset, c + offset, d + offset) | (a, b, c, d) <- quartets]

-- | Replaces list bullets (e.g., '-', '*') with a space if they are
-- at the beginning of the line (ignoring indentation) and followed by a space.
-- Preserves indentation.
maskListBullets :: T.Text -> T.Text
maskListBullets = T.unlines . map replaceBullet . T.lines
  where
    replaceBullet line =
      let (indent, content) = T.span Char.isSpace line
      in case T.uncons content of
           Just (c, rest)
             | c `elem` Const.bullets
             , not (T.null rest)
             , T.head rest == ' ' ->
                 -- Replace the bullet char 'c' with a space to maintain alignment
                 indent <> " " <> rest
           _ -> line
