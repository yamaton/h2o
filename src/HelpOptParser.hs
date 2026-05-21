{-# LANGUAGE DuplicateRecordFields #-}

-- | Parsers for the option-spec part of help text.
--
-- This module owns the grammar for strings such as @--flag@, @-f@,
-- @--output FILE@, @--name=VALUE@, and clap's @--verbose...@ repeat marker.
-- It deliberately does not decide where option text ends and description text
-- begins in raw help output; that fallback line splitting belongs to
-- "HelpLineParser".  Metadata/status classification belongs to "HelpMetadata".
module HelpOptParser where

import Data.Char (isNumber, isSpace, toLower)
import qualified Data.List as List
import Data.List.Extra (dropPrefix, nubOrd, trim)
import qualified HelpMetadata as Metadata
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
  x <- satisfy (\c -> c `elem` alphanumChars ++ "\"`'_^#.@<")
  xs <- munch (\c -> c `elem` (alphanumChars ++ "\"`'_:<>+-*/|#.=@"))
  let res
        | check = pfail
        | map toLower (x : xs) == "or" = pfail -- avoid conflict with "or" in optSep
        | (x : xs) == "Excludes:" = do
            -- A special treatment for micromamba to take strings like
            -- "Excludes: --system --file" as an argument.
            rest <- munch (`notElem` "\n")
            return ((x : xs) ++ rest)
        | otherwise = return (x : xs)
  res

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
  (consumed, _) <- gather $ between (char bra) (char ket) (many (argWordBracketedHelper bra ket <++ nonBracketLetters))
  return consumed
  where
    nonBracketLetters = munch1 (`notElem` ['\n', bra, ket])

argWordBracketedHelperEased :: Char -> Char -> ReadP String
argWordBracketedHelperEased bra ket = do
  (consumed, _) <- gather $ between (char bra) (char ket) (many (argWordBracketedHelper bra ket <++ nonClosingLetters))
  return consumed
  where
    nonClosingLetters = munch1 (`notElem` ['\n', ket])

argWordQuoteHelper :: Char -> ReadP String
argWordQuoteHelper cc = do
  s <- look
  let n = length $ filter (== cc) s
  if n > 8
    then pfail
    else do
      (consumed, _) <- gather $ between (char cc) (char cc) (many nonBracketLetters)
      return consumed
  where
    nonBracketLetters = munch1 (`notElem` ['\n', cc])

argWordBracketed :: ReadP String
argWordBracketed = do
  check <- isLongOptBracketed
  if check
    then pfail
    else argWordAngleBracketed <++ argWordCurlyBracketed <++ argWordParenthesized <++ argWordSquareBracketed <++ argWordDoubleQuoted <++ argWordSingleQuoted

-- People use < and > both as inequalities and brackets.
argWordAngleBracketed :: ReadP String
argWordAngleBracketed = argWordBracketedHelperEased '<' '>'

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
  -- For example stack --help has "--docker*".
  _ <- char '*' <++ pure '*'
  let raw = x : xs
  case stripSuffix "..." raw of
    Just base
      | (not . null) base && isAlphanum (last base) -> return base
    _ ->
      -- Don't allow options like `-S.` or `--baba.`
      if (not . null) xs && last xs == '.'
        then pfail
        else return raw
  where
    stripSuffix suffix str
      | suffix `List.isSuffixOf` str = Just (take (length str - length suffix) str)
      | otherwise = Nothing

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

-- Handle irregular cases like (--help) or [ --baba ].
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

-- Avoid consuming longOptNameBracketed as an argument.
isLongOptBracketed :: ReadP Bool
isLongOptBracketed = (True <$ longOptNameBracketed) <++ pure False

-- | Parses a specific dash string and ensures it is followed by a word boundary.
dashParser :: String -> OptName -> ReadP OptName
dashParser prefix res = do
  _ <- string prefix
  s <- look
  case s of
    (c : _) | isSpace c -> return res
    [] -> return res
    _ -> pfail

doubleDash :: ReadP OptName
doubleDash = dashParser "--" (OptName "--" DoubleDashOnlyType)

singleDash :: ReadP OptName
singleDash = dashParser "-" (OptName "-" SingleDashOnlyType)

shortOptName :: ReadP OptName
shortOptName = do
  _ <- dash
  c <- alphanum +++ satisfy (`elem` "@$=!?&#%~\":._")
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

-- For bazel, disable above and enable below.
-- optName = longOptNameWithNo <++ longOptName <++ oldOptName <++ shortOptName <++ singleDash

optArgBare :: ReadP OptArg
optArgBare = do
  _ <- char '=' <++ singleSpace <++ pure ' '
  _ <- munch (== ' ')
  argWordBare

optArgAsSingleStar :: ReadP OptArg
optArgAsSingleStar = do
  _ <- char '=' <++ singleSpace <++ pure ' '
  argWordSingleStar

optArgInBraket :: ReadP OptArg
optArgInBraket = do
  _ <- char '=' <++ singleSpace <++ pure ' '
  _ <- munch (== ' ')
  x <- argWordBracketed
  postfix <- option "" (string "?")
  return (x ++ postfix)

optArgAsNumber :: ReadP OptArg
optArgAsNumber = do
  _ <- char '='
  _ <- munch (== ' ')
  argWordNumber

optArg :: ReadP OptArg
optArg = optArgInBraket <++ optArgBare <++ optArgAsNumber <++ optArgAsSingleStar

skip :: ReadP a -> ReadP ()
skip a = a *> pure ()

optNameArgPair :: ReadP (OptName, OptArg)
optNameArgPair = do
  name <- optName
  (s, args) <- gather $ sepBy optArg argSep
  extra <- twoOrMoreDots <++ pure ""
  let s' = (trim . dropPrefix "=") s
  let cleanArg = if null args then s' else s' ++ extra
  if argsAreJustOr args || length args >= 5 || cleanArg == "."
    then pfail
    else return (name, cleanArg)
  where
    argsAreJustOr [arg] = trim arg == "or"
    argsAreJustOr _ = False
    twoOrMoreDots = do
      c <- char '.'
      rest <- munch1 (== '.')
      return (c : rest)

-- | Separator of option-argument pairs.
optSep :: ReadP String
optSep = do
  s <- delimiter <++ string " "
  -- Workaround to handle the bug in squashOptions in Layout.
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

-- | Separator between an option and its arguments.
argSep :: ReadP String
argSep = delimiter <++ string " " <++ pure " "
  where
    delimiter = do
      _ <- char ' ' <++ pure ' '
      s <- string ":" <++ string "," <++ string "-" <++ string "|"
      _ <- char ' ' <++ pure ' '
      return s

-- | Extract option names and their argument from an option-spec fragment.
--
-- The first non-empty argument is kept when a fragment has multiple options
-- with arguments, e.g. @-o ARG1, --out=ARG2@.
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

-- | Convert an already separated option fragment and description to Opts.
--
-- This is intentionally strict: it does not fall back to raw-line splitting.
-- Use "HelpLineParser" for raw help lines and the "HelpParser" facade for the
-- historical fallback behavior.
parseOptDescPair :: String -> String -> [Opt]
parseOptDescPair optStr descStr
  | (not . null) res = toOpts res
  | (not . null) resWithoutMetadata = toOpts resWithoutMetadata
  | otherwise = []
  where
    res = readP_to_S optPart optStr
    strippedOptStr = Metadata.dropTrailingTypeMetadata optStr
    resWithoutMetadata =
      if strippedOptStr == optStr
        then []
        else readP_to_S optPart strippedOptStr
    toOpts = map ((\(a, b) -> Opt a (Metadata.stripTrailingArgMetadata b) descStr) . fst)

-- | Convert a separated option fragment without metadata stripping.
--
-- The raw-line fallback historically used this stricter path after it had
-- already split a line into @(optionText, descriptionText)@.
parseRawOptDescPair :: String -> String -> [Opt]
parseRawOptDescPair optStr descStr =
  map ((\(a, b) -> Opt a b descStr) . fst) $
    readP_to_S optPart optStr
