module HelpMetadata
  ( MetadataLineKind (..),
    dropTrailingTypeMetadata,
    isMetadataDescriptionLine,
    isMetadataOnlyLine,
    isMetadataStatusAnnotationLine,
    isOptionLineWithOnlyMetadata,
    isStatusAnnotationLine,
    isTypeMetadataLine,
    looksLikeBareTypeMetadata,
    metadataLineKind,
    spaceRuns,
    stripTrailingArgMetadata,
  )
where

import Data.Char (isAlpha, isAlphaNum, isLower, isNumber, isUpper)
import Data.List (isInfixOf)
import qualified Data.List as List
import Data.List.Extra (trim)
import qualified Data.Maybe as Maybe
import qualified Utils

data MetadataLineKind
  = NotMetadataLine
  | StatusAnnotationLine String
  | MetadataOnlyLine
  | MetadataDescriptionLine Int
  | MetadataStatusLine String
  deriving (Eq, Show)

-- | QIIME/Click can wrap rich type information between the option name and
-- the real description:
--
-- @
--   --i-sequences ARTIFACT FeatureData[Sequence] |
--     FeatureData[ProteinSequence]
--                           The sequences to be aligned.
--   --p-maxiterate INTEGER  Specifies how many iterative refinement cycles are
--     Range(0, None)        performed after the initial progressive alignment.
-- @
--
-- The completion only needs the placeholder argument (ARTIFACT, INTEGER,
-- TEXT, ...), while the bracketed/ranged metadata should neither become the
-- description offset nor be emitted as description text.
looksLikeTypeMetadata :: String -> Bool
looksLikeTypeMetadata s =
  not (null trimmed)
    && not (Utils.startsWithDash trimmed)
    && last trimmed /= '.'
    && (looksLikeShortTypeMetadata || looksLikeQiimeTypeExpression || looksLikeChoiceListStart trimmed || looksLikeChoiceListContinuation trimmed)
  where
    trimmed = trim s
    ws = words trimmed
    looksLikeShortTypeMetadata =
      not (null ws)
        && length ws <= 2
        && any (`elem` trimmed) ("[](){}'\",|" :: String)
    looksLikeQiimeTypeExpression =
      any (`elem` trimmed) ("[]|" :: String)
        && any looksLikeTypeName ws
        && all looksLikeTypeExpressionWord ws
    looksLikeTypeName word' =
      any isUpper word' || any (`elem` word') ("[]" :: String)
    looksLikeTypeExpressionWord "|" = True
    looksLikeTypeExpressionWord word' =
      not (null word')
        && any isAlpha word'
        && any isUpper word'
        && all isTypeExpressionChar word'

looksLikeBareTypeMetadata :: String -> Bool
looksLikeBareTypeMetadata s =
  not (null ws)
    && length ws <= 2
    && all (\word' -> isAllCapsWord word' || isCamelCaseTypeName word') ws
    && all looksLikeBareTypeWord ws
  where
    ws = words (trim s)
    isAllCapsWord word' =
      any isAlpha word'
        && all (\c -> not (isAlpha c) || isUpper c) word'
    isCamelCaseTypeName word' =
      any isLower word'
        && any isUpper word'
        && any isUpper (drop 1 word')
    looksLikeBareTypeWord word' =
      any isAlpha word'
        && last word' /= '.'
        && all (\c -> isAlphaNum c || c `elem` ("[]_." :: String)) word'

looksLikeChoiceListStart :: String -> Bool
looksLikeChoiceListStart s =
  "Choices(" `List.isPrefixOf` s
    && ("," `isInfixOf` s || ")" `List.isSuffixOf` s)
    && all isChoiceListChar s

looksLikeChoiceListContinuation :: String -> Bool
looksLikeChoiceListContinuation "" = False
looksLikeChoiceListContinuation s@(first : _) =
  first `elem` ("'\"" :: String)
    && ("," `isInfixOf` s || ")" `List.isSuffixOf` s)
    && all isChoiceListChar s

isChoiceListChar :: Char -> Bool
isChoiceListChar c =
  isAlphaNum c || c `elem` (" '\"`,()_-./" :: String)

isOptionLineWithOnlyMetadata :: Int -> String -> Bool
isOptionLineWithOnlyMetadata offset line =
  looksLikeTypeMetadata (drop offset line)

statusAnnotation :: String -> Maybe String
statusAnnotation line =
  case trim line of
    "[required]" -> Just "[required]"
    "[optional]" -> Just "[optional]"
    s
      | "[default:" `List.isPrefixOf` s && "]" `List.isSuffixOf` s -> Just s
      | otherwise -> Nothing

isStatusAnnotationLine :: String -> Bool
isStatusAnnotationLine = Maybe.isJust . statusAnnotation

metadataLineKind :: String -> MetadataLineKind
metadataLineKind line
  | Just status <- statusAnnotation line = StatusAnnotationLine status
  | Just status <- metadataStatus = MetadataStatusLine status
  | Just offset <- metadataDescription = MetadataDescriptionLine offset
  | looksLikeMetadataOnly = MetadataOnlyLine
  | otherwise = NotMetadataLine
  where
    metadataStatus =
      Maybe.listToMaybe
        [ status
        | (_, _, suffix) <- metadataSplitCandidates line,
          Just status <- [statusAnnotation suffix]
        ]
    metadataDescription =
      Maybe.listToMaybe
        [ offset
        | (offset, _, suffix) <- metadataSplitCandidates line,
          Maybe.isNothing (statusAnnotation suffix)
        ]
    looksLikeMetadataOnly =
      not (null (trim line))
        && looksLikeTypeMetadata line
        && Maybe.isNothing metadataDescription

metadataSplitCandidates :: String -> [(Int, String, String)]
metadataSplitCandidates line =
  [ (end, prefix, suffix)
  | (start, end) <- spaceRuns line,
    end < length line,
    let prefix = trim (take start line),
    let suffix = trim (drop end line),
    looksLikeTypeMetadata prefix || looksLikeBareTypeMetadata prefix,
    not (null suffix)
  ]

isMetadataStatusAnnotationLine :: String -> Bool
isMetadataStatusAnnotationLine line =
  case metadataLineKind line of
    MetadataStatusLine _ -> True
    _ -> False

spaceRuns :: String -> [(Int, Int)]
spaceRuns x =
  [ (start, end)
  | start <- [0 .. length x - 1],
    x !! start == ' ',
    start == 0 || x !! (start - 1) /= ' ',
    let end = start + length (takeWhile (== ' ') (drop start x)),
    end - start >= 3
  ]

isMetadataDescriptionLine :: Int -> String -> Bool
isMetadataDescriptionLine offset line =
  case metadataLineKind line of
    MetadataDescriptionLine offset' -> offset == offset'
    _ -> False

isMetadataOnlyLine :: String -> Bool
isMetadataOnlyLine line =
  case metadataLineKind line of
    MetadataOnlyLine -> True
    _ -> False

isTypeMetadataLine :: String -> Bool
isTypeMetadataLine line =
  case metadataLineKind line of
    MetadataOnlyLine -> True
    MetadataDescriptionLine _ -> True
    MetadataStatusLine _ -> True
    _ -> False

dropTrailingTypeMetadata :: String -> String
dropTrailingTypeMetadata optStr =
  case words optStr of
    optWithArg : rest
      | Utils.startsWithDash optWithArg
          && "=" `List.isInfixOf` optWithArg
          && isTrailingArgMetadata (unwords rest) ->
          optWithArg
    optName' : arg : rest
      | Utils.startsWithDash optName'
          && not (Utils.startsWithDash arg)
          && isTrailingArgMetadata (unwords rest) ->
          unwords [optName', arg]
    _ -> optStr

stripTrailingArgMetadata :: String -> String
stripTrailingArgMetadata arg =
  case words arg of
    placeholder : rest
      | looksLikeArgPlaceholder placeholder
          && isTrailingArgMetadata (unwords rest) ->
          placeholder
    _ -> arg

looksLikeArgPlaceholder :: String -> Bool
looksLikeArgPlaceholder arg =
  not (null base)
    && any isAlpha base
    && all (\c -> isUpper c || isNumber c || c `elem` ("_-" :: String)) base
  where
    base = reverse $ dropWhile (== '.') $ reverse arg

isTrailingArgMetadata :: String -> Bool
isTrailingArgMetadata s =
  not (null trimmed)
    && ( isChoicesMetadata
           || isRangeMetadata
           || looksLikeTypeExpressionMetadata
       )
  where
    trimmed = trim s
    ws = words trimmed
    isChoicesMetadata = "Choices(" `List.isPrefixOf` trimmed
    isRangeMetadata = "Range(" `List.isPrefixOf` trimmed || "Range(" `List.isInfixOf` trimmed
    looksLikeTypeExpressionMetadata =
      any (`elem` trimmed) ("[]|" :: String)
        && any looksLikeTypeExpressionWord ws
        && all looksLikeTypeExpressionWord ws
    looksLikeTypeExpressionWord "|" = True
    looksLikeTypeExpressionWord word' =
      not (null word')
        && any isAlpha word'
        && any isUpper word'
        && all isTypeExpressionChar word'

isTypeExpressionChar :: Char -> Bool
isTypeExpressionChar c =
  isAlphaNum c || fromEnum c > 127 || c `elem` ("[]_|./+-" :: String)
