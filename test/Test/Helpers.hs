module Test.Helpers
  ( makeOpt
  , test_optPart
  , test_optPartMany
  , test_parser
  , test_parseMany
  , test_parseBlockwise
  , test_parseUsage
  , toLazyByteString
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import HelpParser (optName, optPart)
import qualified Layout
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.ParserCombinators.ReadP (readP_to_S)
import Type (Opt (..), OptName (..))

toLazyByteString :: T.Text -> BL.ByteString
toLazyByteString = TLE.encodeUtf8 . TL.fromStrict

makeOpt :: [String] -> String -> String -> Opt
makeOpt names = Opt (map getOptName names)
  where
    getOptName s = case readP_to_S optName s of
      [(optname, _)] -> optname
      _ -> undefined

test_optPart :: String -> ([String], String) -> TestTree
test_optPart s (names, args) =
  testCase s $ do
    List.sort actual @?= expected
  where
    actual = map ((\(xs, y) -> (map _raw xs, y)) . fst) $ readP_to_S optPart s
    expected = [(names, args)]

test_optPartMany :: String -> [([String], String)] -> TestTree
test_optPartMany s pairs =
  testCase s $ do
    actual @?= expected
  where
    actual = List.sort $ map ((\(xs, y) -> (map show xs, y)) . fst) $ readP_to_S optPart s
    expected = List.sort pairs

test_parseBlockwise :: String -> [Opt] -> TestTree
test_parseBlockwise s opts =
  testCase s $ do
    actual @?= expected
  where
    actual = List.sort $ Layout.parseBlockwise s
    expected = List.sort opts

test_parser :: String -> ([String], String, String) -> TestTree
test_parser s (names, args, desc) =
  testCase s $ do
    actual @?= expected
  where
    actual = List.sort $ Layout.parseMany s
    expected = List.sort [makeOpt names args desc]

test_parseMany :: String -> [([String], String, String)] -> TestTree
test_parseMany s tuples =
  testCase s $ do
    actual @?= expected
  where
    actual = List.sort $ Layout.parseMany s
    expected = List.sort [makeOpt names args desc | (names, args, desc) <- tuples]

test_parseUsage :: String -> String -> TestTree
test_parseUsage s expected =
  testCase s $ do
    actual @?= expected
  where
    actual = Layout.parseUsage s
