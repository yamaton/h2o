-- | Fallback parser for raw help lines and paragraphs.
--
-- This module owns the heuristic that splits raw help text into
-- @(optionText, descriptionText)@ pairs when the column layout parser cannot
-- do it.  It does not define the grammar of option names or arguments; those
-- are delegated to "HelpOptParser".
module HelpLineParser
  ( fallback,
    parseLine,
    preprocessor,
    preprocessAllFallback,
  )
where

import Data.List.Extra (nubOrd)
import qualified HelpOptParser as OptParser
import Text.ParserCombinators.ReadP
import Type (Opt)
import qualified Utils

-- Very heuristic handling for separating the option part from the description.
heuristicSep :: String -> ReadP String
heuristicSep args =
  f ":\n" <++ f "\n" <++ f ": " <++ f "\t" <++ twoOrMoreSpaces <++ varSpaces
  where
    f s = (OptParser.singleSpace <++ pure ' ') *> string s
    twoOrMoreSpaces = string " " *> munch1 (== ' ')
    varSpaces
      | null args = twoSpaces
      | last args `elem` ">}])" = oneSpace
      | otherwise = twoSpaces
    twoSpaces = string "  "
    oneSpace = string " "

failWithBracket :: ReadP String
failWithBracket = do
  (s, _) <- gather (sepBy1 w (OptParser.singleSpace +++ char ':'))
  return s
  where
    w = munch1 (`notElem` " :[]\n\t")

beforeSquareBraket :: ReadP String
beforeSquareBraket = do
  s <- option "" failWithBracket
  space <- option "" (string " ")
  rest <- look
  case rest of
    "" -> pure ()
    '[' : _ -> pure ()
    _ -> pfail
  return (s ++ space)

afterSquareBraket :: ReadP String
afterSquareBraket = do
  space <- option "" (string " ")
  s <- option "" failWithBracket
  return (space ++ s)

discardSquareBracket :: ReadP String
discardSquareBracket = do
  first <- beforeSquareBraket
  _ <- surroundedBySquareBracket
  second <- afterSquareBraket
  return (first ++ second)

unwrapSquareBracket :: ReadP String
unwrapSquareBracket = do
  first <- beforeSquareBraket
  content <- surroundedBySquareBracket
  second <- afterSquareBraket
  return (first ++ content ++ second)

surroundedBySquareBracket :: ReadP String
surroundedBySquareBracket = do
  between (char '[') (char ']') nonBracketLettersForSure
  where
    nonBracketLettersForSure = munch1 (`notElem` "[]\n")

squareBracketHandler :: ReadP String
squareBracketHandler = do
  x <- OptParser.dash
  xs <- choice [failWithBracket, discardSquareBracket, unwrapSquareBracket]
  return (x : xs)

-- | Extract an @(optionText, descriptionText) pair from a raw help line.
--
-- The option text is left as text; "HelpOptParser" is responsible for parsing
-- it into option names and arguments afterwards.
preprocessor :: ReadP (String, String)
preprocessor = do
  skipSpaces
  (consumed, opt) <- gather squareBracketHandler
  _ <- heuristicSep consumed
  skipSpaces
  desc <- munch1 ('\n' /=)
  OptParser.skip OptParser.newline <++ eof
  return (opt, desc)

fallback :: ReadP (String, String)
fallback = do
  _ <- munch ('\n' /=)
  OptParser.skip OptParser.newline
  return ("", "")

-- | Parse Opts from a raw single-line help fragment.
parseLine :: String -> [Opt]
parseLine s = nubOrd . concat $ results
  where
    pairs = map fst $ readP_to_S preprocessor s
    results =
      [ (\ys -> if null ys then Utils.warnShow "⚠️Failed pair (parseLine)⚠️\n" (optStr, descStr) ys else ys) $
          OptParser.parseRawOptDescPair optStr descStr
      | (optStr, descStr) <- pairs
      ]

preprocessAllFallback :: String -> [(String, String)]
preprocessAllFallback "" = []
preprocessAllFallback s = filter (\pair -> pair /= ("", "")) result
  where
    result = case readP_to_S (preprocessor <++ fallback) s of
      [] -> []
      (pair, rest) : moreMatches -> (pair : map fst moreMatches) ++ preprocessAllFallback rest
