{-# LANGUAGE DuplicateRecordFields #-}

module HelpParser where

import qualified Data.List as List
import Data.List.Extra (dropPrefix, nubOrd, trim)
import Debug.Trace (trace)
import Text.ParserCombinators.ReadP
import Type
  ( Opt (..),
    OptName (..),
    OptNameType (..),
  )
import Data.Char (isNumber)

type OptArg = String

digitChars :: [Char]
digitChars = "0123456789"

alphChars :: [Char]
alphChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphanumChars :: [Char]
alphanumChars = alphChars ++ digitChars

dash :: ReadP Char
dash = satisfy (== '-')

alphanum :: ReadP Char
alphanum = satisfy $ \c -> c `elem` alphanumChars

singleSpace :: ReadP Char
singleSpace = char ' '

isAlphanum :: Char -> Bool
isAlphanum c = c `elem` alphanumChars

isAllowedOptChar :: Char -> Bool
isAllowedOptChar c = c `elem` (alphanumChars ++ "+-_!?@.")

newline :: ReadP Char
newline = char '\n'

word :: ReadP String
word = munch1 (`notElem` " \t\n")

argWordBare :: ReadP String
argWordBare = do
  x <- satisfy (\c -> c `elem` alphanumChars ++ "\"`'_^(#.[")
  xs <- munch (\c -> c `elem` (alphanumChars ++ "\"`'_:<>()+-*/|#.=[]"))
  return (x : xs)

argWordNumber :: ReadP String
argWordNumber = do
  sign <- string "-" <++ pure ""
  digits <- munch isNumber
  dot <- string "." <++ pure ""
  extradigits <- munch isNumber <++ pure ""
  return $ concat [sign, digits, dot, extradigits]

argWordBracketedHelper :: Char -> Char -> ReadP String
argWordBracketedHelper bra ket = do
  (consumed, _) <- gather $ between (char bra) (char ket) (many1 (argWordBracketedHelper bra ket +++ nonBracketLettersForSure))
  return consumed
  where
    nonBracketLettersForSure = munch1 (`notElem` ['\n', bra, ket])

argWordBracketed :: ReadP String
argWordBracketed = argWordAngleBracketed <++ argWordCurlyBracketed <++ argWordParenthesized <++ argWordSquareBracketed <++ argWordDoubleQuoted <++ argWordSingleQuoted

argWordAngleBracketed :: ReadP String
argWordAngleBracketed = argWordBracketedHelper '<' '>'

argWordCurlyBracketed :: ReadP String
argWordCurlyBracketed = argWordBracketedHelper '{' '}'

argWordParenthesized :: ReadP String
argWordParenthesized = argWordBracketedHelper '(' ')'

argWordSquareBracketed :: ReadP String
argWordSquareBracketed = argWordBracketedHelper '[' ']'

argWordDoubleQuoted :: ReadP String
argWordDoubleQuoted = argWordBracketedHelper '"' '"'

argWordSingleQuoted :: ReadP String
argWordSingleQuoted = argWordBracketedHelper '\'' '\''

description :: ReadP String
description = do
  xs <- sepBy1 word (munch1 (== ' '))
  return (unwords xs)

optWord :: ReadP String
optWord = do
  x <- alphanum
  xs <- munch isAllowedOptChar
  -- For example docker run --help has "--docker*"
  _ <- char '*' <++ pure '*'
  return (x : xs)

longOptName :: ReadP OptName
longOptName = do
  _ <- count 2 dash
  name <- optWord
  let res = OptName ("--" ++ name) LongType
  return res

doubleDash :: ReadP OptName
doubleDash = do
  _ <- count 2 dash
  let res = OptName "--" DoubleDashAlone
  s <- look
  if null s || head s `elem` " "
    then return res
    else pfail

singleDash :: ReadP OptName
singleDash = do
  _ <- char '-'
  let res = OptName "-" SingleDashAlone
  s <- look
  if null s || head s `elem` " "
    then return res
    else pfail

shortOptName :: ReadP OptName
shortOptName = do
  _ <- dash
  c <- alphanum +++ satisfy (`elem` "@$=?&#%~\":.")
  let res = OptName ('-' : c : "") ShortType
  return res

oldOptName :: ReadP OptName
oldOptName = do
  _ <- dash
  x <- alphanum
  xs <- munch1 isAllowedOptChar
  let res = OptName ('-' : x : xs) OldType
  return res

optName :: ReadP OptName
optName = longOptName <++ doubleDash <++ oldOptName <++ shortOptName <++ singleDash

optArg :: ReadP String
optArg = do
  _ <- char '=' <++ singleSpace <++ pure ' '
  _ <- munch (== ' ')
  argWordBare

optArgInBraket :: ReadP String
optArgInBraket = do
  _ <- char '=' <++ singleSpace <++ pure ' ' -- ok not to have a delimiter before
  _ <- munch (== ' ')
  argWordBracketed

optArgAsNumber :: ReadP String
optArgAsNumber = do
  _ <- char '='
  _ <- munch (== ' ')
  argWordNumber

skip :: ReadP a -> ReadP ()
skip a = a *> pure ()

-- very heuristic handling in separating description part
heuristicSep :: String -> ReadP String
heuristicSep args =
  f ":\n" <++ f "\n" <++ f ": " <++ f "\t" <++ twoOrMoreSpaces <++ varSpaces
  where
    f s = optional singleSpace *> string s
    twoOrMoreSpaces = string " " *> munch1 (== ' ')
    varSpaces
      | null args = twoSpaces
      | last args `elem` ">}])" = oneSpace
      | otherwise = twoSpaces
    twoSpaces = string "  "
    oneSpace = string " "

optNameArgPair :: ReadP (OptName, String)
optNameArgPair = do
  name <- optName
  (s, args) <- gather $ sepBy (optArgInBraket <++ optArg <++ optArgAsNumber) argSep
  extra <- twoOrMoreDots <++ pure ""
  let s' = trim $ dropPrefix "=" s
  if (length args == 1 && trim (head args) == "or") || length args >= 5
    then pfail
    else return (name, s' ++ extra)
  where
    twoOrMoreDots = do
      c <- char '.'
      rest <- munch1 (== '.')
      return (c : rest)

optSep :: ReadP String
optSep = do
  s <- delimiter <++ string " "
  -- following is a workaround to handle the bug in squashOptions in Layout
  _ <- munch (== ' ')
  return s
  where
    modComma = do
      s <- string ","
      _ <- char ',' <++ pure 'x'
      return s
    delimiter = do
      _ <- char ' ' <++ pure 'x'
      modComma <++ string "/" <++ string "|" <++ string "or"

argSep :: ReadP String
argSep = delimiter <++ string " " <++ pure " "
  where
    delimiter = do
      _ <- char ' ' <++ pure ' '
      s <- string ":" <++ string "," <++ string "-" <++ string "|"
      _ <- char ' ' <++ pure ' '
      return s

surroundedBySquareBracket :: ReadP String
surroundedBySquareBracket = do
  between (char '[') (char ']') nonBracketLettersForSure
  where
    nonBracketLettersForSure = munch1 (`notElem` "[]\n")

failWithBracket :: ReadP String
failWithBracket = do
  (s, _) <- gather (sepBy1 w (singleSpace +++ char ':'))
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

squareBracketHandler :: ReadP String
squareBracketHandler = do
  x <- dash -- let if fail if not starting with '-'
  xs <- choice [failWithBracket, discardSquareBracket, unwrapSquareBracket]
  return (x : xs)

-- Extract (optionPart, description) matches
preprocessor :: ReadP (String, String)
preprocessor = do
  skipSpaces
  (consumed, opt) <- gather squareBracketHandler
  _ <- heuristicSep consumed -- this is the separator between optionPart and description
  skipSpaces
  desc <- munch1 ('\n' /=)
  skip newline <++ eof
  return (opt, desc)

fallback :: ReadP (String, String)
fallback = do
  _ <- munch ('\n' /=)
  skip newline
  return ("", "")

-- takes description as external info
-- the first option argument ARG1 is extracted when you have a case like
--    "-o ARG1, --out=ARG2"
optPart :: ReadP ([OptName], OptArg)
optPart = do
  skipSpaces
  pairs <- sepBy1 optNameArgPair optSep
  let names = nubOrd $ map fst pairs
  let args = case filter (not . null) (map snd pairs) of
        [] -> ""
        x : _ -> x
  skipSpaces
  eof
  return (names, args)

parseLine :: String -> [Opt]
parseLine s = List.nub . concat $ results
  where
    -- thanks to lazy evaluation, desc is NOT evaluated when xs == []
    -- so don't worry about calling (head xs).
    xs = readP_to_S preprocessor s
    pairs = map fst xs
    results =
      [ (\ys -> if null ys then trace ("Failed pair: " ++ show (optStr, descStr)) ys else ys) $
          map ((\(a, b) -> Opt a b descStr) . fst) $
            readP_to_S optPart optStr
        | (optStr, descStr) <- pairs
      ]

-- | Parse when options+args and description are given separately
-- Use optPart directly, but go back to preprocessor when
-- the parse fails. Failure happens mostly when the option name
-- contains [] such as `--[no-]copy`.
parseWithOptPart :: String -> String -> [Opt]
parseWithOptPart optStr descStr
  | (not . null) res = map ((\(a, b) -> Opt a b descStr) . fst) res
  | otherwise = trace "[warn] optPart fallback" $ parseLine (optStr ++ "   " ++ descStr) -- fallback
  where
    res = readP_to_S optPart optStr

preprocessAllFallback :: String -> [(String, String)]
preprocessAllFallback "" = []
preprocessAllFallback s = case readP_to_S (preprocessor <++ fallback) s of
  [] -> []
  (pair, rest) : moreMatches -> (pair : map fst moreMatches) ++ preprocessAllFallback rest
