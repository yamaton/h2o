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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.Extra (nubSort, trimStart)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
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
-- as it handles soft hyphen logic. A trailing "--no-" option fragment is
-- treated as hard hyphenation, because argparse help can wrap option names
-- inside descriptions.
smartUnwords :: [String] -> String
smartUnwords [] = ""
smartUnwords xs =
  foldr1 f xs
  where
    hyphens = ['-', '\8208']
    f "" acc = acc
    f s "" = s
    f s acc
      | endsWithHyphen s && endsWithNoOptionFragment s = s ++ acc
    f s acc
      | length s > 1 && c `elem` hyphens && c2 `List.notElem` (' ' : hyphens) = initS ++ acc
      | otherwise = s ++ (' ' : acc)
      where
        c = last s
        initS = init s
        c2 = last initS
    endsWithHyphen s = length s > 1 && last s `elem` hyphens
    endsWithNoOptionFragment s =
      case words s of
        [] -> False
        ws -> "--no-" `List.isPrefixOf` last ws

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

-- | Detect whether a command output starts with a diagnostic/error page rather
-- than real help. The check intentionally uses anchored/specific patterns
-- instead of broad words like "missing" so normal help summaries are not
-- rejected.
hasErrorMessageAtTop :: Text -> Text -> Bool
hasErrorMessageAtTop _name text =
  case T.lines (T.stripStart text) of
    [] -> False
    (l : _) -> hasErrorMessageLine l
  where
    hasErrorMessageLine line =
      not (Utils.isUsageBlock lowered)
        && (hasErrorPrefix || hasErrorPhrase)
      where
        lowered = T.toLower . T.strip . T.take 100 $ line
        candidates = lowered : maybe [] (: []) (dropCommandPrefix lowered)
        hasErrorPrefix =
          any (\prefix -> any (prefix `T.isPrefixOf`) candidates) Const.errPrefixes
        hasErrorPhrase = any (`T.isInfixOf` lowered) Const.errPhrases

    dropCommandPrefix line =
      case T.breakOn ":" line of
        (_, rest)
          | T.null rest -> Nothing
          | otherwise -> Just (T.strip (T.drop 1 rest))

mayContainUseful :: Text -> Text -> Bool
mayContainUseful name text
  | null xs = False
  | [line] <- xs = "usage" `T.isPrefixOf` (T.toLower . T.stripStart) line
  | name == "gatk" = length xs >= 4 -- special handling for GATK
  | otherwise = True
  where
    xs = filter (isNotNullAndErrorMessageAbsent name) . T.lines $ text

-- | Check if a text is free from error-like words at the bottom of the page.
isNotNullAndErrorMessageAbsent :: Text -> Text -> Bool
isNotNullAndErrorMessageAbsent _name text =
  isNotNull && not (hasErrorMessageAtTop "" bottomLine)
  where
    lowered = (T.toLower . T.strip) text
    isNotNull = (not . T.null) lowered
    bottomLine = last (T.lines lowered)

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

-- | 90th percentile of the given values, or 'Nothing' if the list is empty.
-- The previous implementation crashed on empty input; callers are expected
-- to fall back to a sensible default via 'Data.Maybe.fromMaybe'.
topTenPercentile :: (Ord a) => [a] -> Maybe a
topTenPercentile [] = Nothing
topTenPercentile xs = Just (sortedXs !! idx)
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

-- | Remove backspace overstrikes used for bold/underline in terminals and man pages.
-- Handles the char + backspace pattern: when \x08 is encountered, drop it and the preceding character.
removeBackspaceOverstrikes :: Text -> Text
removeBackspaceOverstrikes = T.pack . go [] . T.unpack
  where
    go acc [] = reverse acc
    go [] ('\x08' : cs) = go [] cs
    go (_ : acc) ('\x08' : cs) = go acc cs
    go acc (c : cs) = go (c : acc) cs

-- | Strip ANSI CSI escape sequences per ECMA-48.
-- Matches ESC [ + parameter bytes (0x30-0x3F) + intermediate bytes (0x20-0x2F) + final byte (0x40-0x7E).
stripAnsiEscapes :: Text -> Text
stripAnsiEscapes = T.pack . go . T.unpack
  where
    go [] = []
    go ('\x1B' : '[' : cs) = go (dropCsi cs)
    go ('\x1B' : cs) = go cs
    go (c : cs) = c : go cs
    dropCsi [] = []
    dropCsi (c : cs)
      | '\x30' <= c && c <= '\x3F' = dropCsi cs   -- parameter bytes
      | '\x20' <= c && c <= '\x2F' = dropCsi cs   -- intermediate bytes
      | '\x40' <= c && c <= '\x7E' = cs            -- final byte: consume and stop
      | otherwise = c : cs                          -- malformed: stop

-- | Clean terminal output by removing backspace overstrikes and ANSI escape sequences.
cleanTerminalOutput :: Text -> Text
cleanTerminalOutput = stripAnsiEscapes . removeBackspaceOverstrikes

-- | Decode a lazy 'BSL.ByteString' as UTF-8, substituting U+FFFD for any
-- invalid byte. Help and man output from real-world commands is not always
-- valid UTF-8 - Latin-1 locales, embedded binary noise on stderr, and older
-- groff man pages on macOS all produce byte sequences that strict decoding
-- surfaces as 'Data.Text.Encoding.Error.UnicodeException'. Using lenient
-- decoding here keeps the parser pipeline total without silently dropping
-- the valid surrounding text.
decodeUtf8Lenient :: BSL.ByteString -> Text
decodeUtf8Lenient = TL.toStrict . TLE.decodeUtf8With lenientDecode

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
    step x ([] : gs) = [x] : gs
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
