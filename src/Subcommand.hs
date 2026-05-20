module Subcommand where

import Control.Monad (liftM2)
import Data.Char (toLower)
import qualified Data.List as List
import Data.List.Extra (trim)
import qualified Data.Maybe as Maybe
import HelpParser (alphanumChars, newline, singleSpace, skip, word)
import Layout (getDescriptionOffset)
import Text.ParserCombinators.ReadP
import Type (Subcommand (..))
import Utils (infoMsg)
import qualified Utils

type Layout = (Int, Int)

-- | Returns location of first two words:
-- -1 if the first or the second word unavailable
--    firstTwoWordsLoc "  hello" == (2, -1)
--    firstTwoWordsLoc "       " == (-1, -1)
firstTwoWordsLoc :: String -> (Int, Int)
firstTwoWordsLoc line = (firstLoc, secondLoc)
  where
    (spaces, rest0) = span (' ' ==) line
    (w, rest1) = span (' ' /=) rest0
    (midSpaces, rest2) = span (' ' ==) rest1
    firstLoc = if null w then -1 else length spaces
    secondLoc = if null rest2 then -1 else firstLoc + length w + length midSpaces

getLayoutMaybe :: [String] -> Int -> Maybe Layout
getLayoutMaybe xs offset = liftM2 (,) first second
  where
    pairs =
      infoMsg "First two word locations:" $
        filter (\(a, b) -> a > 0 && b >= a + 6 && a < offset) $
          map firstTwoWordsLoc xs
    second = Utils.getMostFrequent [b | (_, b) <- pairs]
    first = Utils.getMostFrequent [a | (a, _) <- pairs, a < Maybe.fromMaybe 50 second]

getAlignedLines :: String -> [String]
getAlignedLines s =
  case layoutMay of
    Just lay -> foldWrappedDescriptions lay xs
    _ -> []
  where
    xs = filter removeJunkDashLine (lines s)
    offsetMay = getDescriptionOffset (unlines xs)
    offset = infoMsg "Description offset:" $ Maybe.fromMaybe 50 offsetMay
    ys = filter removeJunkLine (lines s)
    layoutMay = infoMsg "Detected layout:" $ getLayoutMaybe ys offset

-- | Merge physical continuation lines into the preceding subcommand row.
--
-- Click-style help, including QIIME 2, wraps long descriptions by aligning the
-- following line with the description column:
--
-- @
--   cutadapt            Plugin for removing adapter sequences, primers, and
--                       other unwanted sequence from sequence data.
-- @
--
-- Only rows that already match the detected subcommand layout can start a
-- logical line. Continuations must be immediately adjacent and begin at or
-- beyond the description column, so this does not introduce new command
-- candidates.
foldWrappedDescriptions :: Layout -> [String] -> [String]
foldWrappedDescriptions layout@(_, descOffset) = reverse . flush . foldl step (Nothing, [])
  where
    step (current, acc) line
      | isSubcommandRow line = (Just line, maybe acc (: acc) current)
      | isContinuation line = (appendContinuation line <$> current, acc)
      | otherwise = (Nothing, maybe acc (: acc) current)

    flush (current, acc) = maybe acc (: acc) current

    isSubcommandRow line = firstTwoWordsLoc line == layout

    isContinuation line =
      case trim line of
        "" -> False
        _ ->
          getIndentation line >= descOffset
            && firstTwoWordsLoc line /= layout
            && removeJunkLine line
            && removeJunkDashLine line

    appendContinuation line current = current ++ " " ++ trim line

    getIndentation = length . takeWhile (== ' ')

lowercase :: String
lowercase = "abcdefghijklmnopqrstuvwxyz"

isAlphanumOrDashOrUnderscore :: Char -> Bool
isAlphanumOrDashOrUnderscore c = c `elem` ('-' : '_' : alphanumChars)

subcommandWord :: ReadP String
subcommandWord = do
  -- [NOTE] Assume subcommand starts with lowercase
  -- Replace with the commented line if you want (uppercase OR lowercase) instead
  x <- satisfy $ \c -> toLower c `elem` lowercase
  -- x <- satisfy $ \c -> c `elem` alphChars  --
  xs <- munch isAlphanumOrDashOrUnderscore
  -- Discard postfix '*'.
  -- Never seen subcommand followed by * as special note,
  -- but there exists options like --docker* as in `stack --help`.
  _ <- char '*' <++ pure '*'
  return (x : xs)

subcommand :: ReadP Subcommand
subcommand = do
  skipSpaces
  name <- subcommandWord
  _ <- subcommandSep
  ss <- sepBy1 word singleSpace
  _ <- munch (== ' ')
  skip newline <++ eof
  let desc = unwords ss
  return (Subcommand name desc)

subcommandSep :: ReadP String
subcommandSep = colonBased <++ spaceBased
  where
    colonBased = do
      skipSpaces
      s <- string ":"
      skipSpaces
      return s
    spaceBased = do
      s <- string " "
      skipSpaces
      return s

removeJunkDashLine :: String -> Bool
removeJunkDashLine s =
  (not . List.isPrefixOf "- " $ ss)
    && (not . List.isPrefixOf "-- " $ ss)
    && (not . List.isPrefixOf "---" $ ss)
  where
    ss = trim s

removeJunkLine :: String -> Bool
removeJunkLine s =
  (not . null . trim $ s)
    && (not . Utils.startsWithLongOption $ s)
    && (not . Utils.startsWithShortOrOldOption $ s)
    && (not . Utils.startsWithChar '[' $ s)

parseSubcommand :: String -> [Subcommand]
parseSubcommand content = infoMsg "Parsed subcommands:" results
  where
    xs = getAlignedLines content
    results = (map (fst . last) . filter (not . null)) [readP_to_S subcommand x | x <- xs]
