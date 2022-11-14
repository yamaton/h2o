{-# LANGUAGE DuplicateRecordFields #-}

module HelpParser where

import Data.Char (isNumber, toLower)
import qualified Data.List as List
import Data.List.Extra (dropPrefix, nubOrd, trim)
import Debug.Trace (trace)
import Text.ParserCombinators.ReadP
import Type
  ( Opt (..),
    OptName (..),
    OptNameType (..),
  )

type OptArg = String

digitChars :: String
digitChars = "0123456789"

alphChars :: String
alphChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

symbolChars :: String
symbolChars = "+-_!?@."

alphanumChars :: String
alphanumChars = alphChars ++ digitChars

dash :: ReadP Char
dash = char '-'

alphanum :: ReadP Char
alphanum = satisfy $ \c -> c `elem` alphanumChars

singleSpace :: ReadP Char
singleSpace = char ' '

isAlphanum :: Char -> Bool
isAlphanum c = c `elem` alphanumChars

isAllowedOptChar :: Char -> Bool
isAllowedOptChar c = c `elem` (alphanumChars ++ symbolChars)

newline :: ReadP Char
newline = char '\n'

word :: ReadP String
word = munch1 (`notElem` " \t\n")

argWordBare :: ReadP String
argWordBare = do
  check <- isLongOptBracketed
  x <- satisfy (\c -> c `elem` alphanumChars ++ "\"`'_^(#.[@")
  xs <- munch (\c -> c `elem` (alphanumChars ++ "\"`'_:<>()+-*/|#.=[]@"))
  if check
    then pfail
    else
      if (x : xs) == "Excludes:"
        then do
          -- a special treatment for micromamba to take string
          -- like "Excludes: --system --file" as an argument
          rest <- munch (`notElem` "\n")
          return ((x : xs) ++ rest)
        else return (x : xs)

argWordSingleStar :: ReadP String
argWordSingleStar = string "*"

argWordNumber :: ReadP String
argWordNumber = do
  sign <- string "-" <++ pure ""
  digits <- munch isNumber
  dot <- string "." <++ pure ""
  extradigits <- munch isNumber <++ pure ""
  return $ concat [sign, digits, dot, extradigits]

argWordBracketedHelper :: Char -> Char -> ReadP String
argWordBracketedHelper bra ket = do
  (consumed, _) <- gather $ between (char bra) (char ket) (many1 (argWordBracketedHelper bra ket <++ nonBracketLettersForSure))
  return consumed
  where
    nonBracketLettersForSure = munch1 (`notElem` ['\n', bra, ket])

argWordQuoteHelper :: Char -> ReadP String
argWordQuoteHelper cc = do
  s <- look
  let n = length $ filter (== cc) s
  if n > 8
    then pfail
    else do
      (consumed, _) <- gather $ between (char cc) (char cc) (many nonBracketLettersForSure)
      return consumed
  where
    nonBracketLettersForSure = munch1 (`notElem` ['\n', cc])

argWordBracketed :: ReadP String
argWordBracketed = do
  check <- isLongOptBracketed
  if check
    then pfail
    else argWordAngleBracketed <++ argWordCurlyBracketed <++ argWordParenthesized <++ argWordSquareBracketed <++ argWordDoubleQuoted <++ argWordSingleQuoted

argWordAngleBracketed :: ReadP String
argWordAngleBracketed = argWordBracketedHelper '<' '>'

argWordCurlyBracketed :: ReadP String
argWordCurlyBracketed = argWordBracketedHelper '{' '}'

argWordParenthesized :: ReadP String
argWordParenthesized = argWordBracketedHelper '(' ')'

argWordSquareBracketed :: ReadP String
argWordSquareBracketed = argWordBracketedHelper '[' ']'

argWordDoubleQuoted :: ReadP String
argWordDoubleQuoted = argWordQuoteHelper '"'

argWordSingleQuoted :: ReadP String
argWordSingleQuoted = argWordQuoteHelper '\''

description :: ReadP String
description = do
  xs <- sepBy1 word (munch1 (== ' '))
  return (unwords xs)

optWord :: ReadP String
optWord = do
  x <- alphanum
  xs <- munch isAllowedOptChar
  -- For example stack --help has "--docker*"
  _ <- char '*' <++ pure '*'
  -- Don't allow options like `-S.` or `--baba.`
  if (not . null) xs && last xs == '.'
    then pfail
    else return (x : xs)

longOptNameWithNo :: ReadP OptName
longOptNameWithNo = do
  _ <- count 2 dash
  _ <- string "[no]"
  name <- optWord
  let res = OptName ("--" ++ name) LongType
  return res

longOptName :: ReadP OptName
longOptName = do
  _ <- count 2 dash
  name <- optWord
  let res = OptName ("--" ++ name) LongType
  return res

-- Handle irregular case like (--help) or [ --baba ]
longOptNameBracketed :: ReadP OptName
longOptNameBracketed =
  longOptNameBracketedHelper '(' ')' <++ longOptNameBracketedHelper '[' ']'

longOptNameBracketedHelper :: Char -> Char -> ReadP OptName
longOptNameBracketedHelper bra ket = do
  _ <- char bra
  _ <- singleSpace <++ pure ' '
  res <- longOptName
  _ <- singleSpace <++ pure ' '
  _ <- char ket
  return res

-- ugly hack to avoid consumption of longOptNameBracketed as an argument
isLongOptBracketed :: ReadP Bool
isLongOptBracketed = (True <$ longOptNameBracketed) <++ pure False

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
  let res = OptName ['-', c] ShortType
  return res

oldOptName :: ReadP OptName
oldOptName = do
  _ <- dash
  name <- optWord
  let res = OptName ('-' : name) OldType
  if length name >= 2
    then return res
    else pfail

optName :: ReadP OptName
optName = longOptName <++ doubleDash <++ oldOptName <++ shortOptName <++ singleDash <++ longOptNameBracketed

-- For bazel, disable above and enable below
-- optName = longOptNameWithNo <++ longOptName <++ oldOptName <++ shortOptName <++ singleDash

optArg :: ReadP OptArg
optArg = do
  _ <- char '=' <++ singleSpace <++ pure ' '
  _ <- munch (== ' ')
  argWordBare

optArgAsSingleStar :: ReadP OptArg
optArgAsSingleStar = do
  _ <- char '=' <++ singleSpace <++ pure ' '
  argWordSingleStar

optArgInBraket :: ReadP OptArg
optArgInBraket = do
  _ <- char '=' <++ singleSpace <++ pure ' ' -- ok not to have a delimiter before
  _ <- munch (== ' ')
  argWordBracketed

optArgAsNumber :: ReadP OptArg
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

optNameArgPair :: ReadP (OptName, OptArg)
optNameArgPair = do
  name <- optName
  (s, args) <- gather $ sepBy (optArgInBraket <++ optArg <++ optArgAsNumber <++ optArgAsSingleStar) argSep
  extra <- twoOrMoreDots <++ pure ""
  let s' = (trim . dropPrefix "=") s --- Exclude ':' as the last letter of an argument
  let cleanArg = s' ++ extra
  if (length args == 1 && trim (head args) == "or") || length args >= 5 || cleanArg == "."
    then pfail
    else return (name, cleanArg)
  where
    twoOrMoreDots = do
      c <- char '.'
      rest <- munch1 (== '.')
      return (c : rest)

-- | Separator of option-argument pairs
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

-- | Separator between an option and its arguments
argSep :: ReadP String
argSep = delimiter <++ string " " <++ pure " "
  where
    delimiter = do
      _ <- char ' ' <++ pure ' '
      s <- string ":" <++ string "," <++ string "-" <++ string "|"
      _ <- char ' ' <++ pure ' '
      return s

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

surroundedBySquareBracket :: ReadP String
surroundedBySquareBracket = do
  between (char '[') (char ']') nonBracketLettersForSure
  where
    nonBracketLettersForSure = munch1 (`notElem` "[]\n")

squareBracketHandler :: ReadP String
squareBracketHandler = do
  x <- dash -- let if fail if not starting with '-'
  xs <- choice [failWithBracket, discardSquareBracket, unwrapSquareBracket]
  return (x : xs)

-- | Extract (optionPart, description) matches
-- Here optionPart includes options and arguments.
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

-- | Extracts optNames and OptArg
-- [NOTE] the first OptArg is kept when there are multiple ones
-- For example, ARG1 is kept when  "-o ARG1, --out=ARG2"
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

-- | Parse Opt from single-line string
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

-- | Get `Opt`s from (options+args) string and (description) string
parseWithOptPart :: String -> String -> [Opt]
parseWithOptPart optStr descStr
  | (not . null) res = map ((\(a, b) -> Opt a b descStr) . fst) res
  | otherwise = trace "[warn] optPart fallback" $ parseLine (optStr ++ "   " ++ descStr) -- fallback
  where
    res = readP_to_S optPart optStr

preprocessAllFallback :: String -> [(String, String)]
preprocessAllFallback "" = []
preprocessAllFallback s = filter (\pair -> pair /= ("", "")) result
  where
    result = case readP_to_S (preprocessor <++ fallback) s of
      [] -> []
      (pair, rest) : moreMatches -> (pair : map fst moreMatches) ++ preprocessAllFallback rest

-- | Extract the header title line IF it the it contains one of the given keywords "properly"
-- [NOTE] Assumes the first line of block as a header line
parseUsageHeader :: [String] -> String -> Maybe String
parseUsageHeader _ "" = Nothing
parseUsageHeader [] _ = Nothing
parseUsageHeader keywords block
  | null pairs = Nothing
  | otherwise = (Just . fst . head) pairs
  where
    kwds = map (map toLower) keywords
    headerLine = map toLower (head (lines block))
    pairs = readP_to_S (headerWithOffset kwds) headerLine

headerWithOffset :: [String] -> ReadP String
headerWithOffset [] = pfail
headerWithOffset names = do
  preSpaces <- munch (== ' ')
  nameFound <- foldr1 (<++) (map string names)
  postSpaces <- munch (== ' ')
  colon <- string ":" <++ pure ""
  aftSpaces <- munch (== ' ')
  let components = [preSpaces, nameFound, postSpaces, colon, aftSpaces]
  return (concat components)
