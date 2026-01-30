module Test.PropertyTests (tests) where

import Data.List.Extra (nubSort)
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HelpParser (optName)
import qualified Layout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.ParserCombinators.ReadP (readP_to_S)
import Type (OptName (..), OptNameType (..))

tests :: TestTree
tests =
  testGroup
    "Hedgehog tests"
    [ testProperty "optName (long)" prop_longOpt,
      testProperty "optName (short)" prop_shortOpt,
      testProperty "optName (old)" prop_oldOpt,
      testProperty "merge ranges" prop_mergeRanges
    ]

prop_longOpt :: Property
prop_longOpt =
  property $ do
    s <- forAll (Gen.string (Range.constant 1 10) Gen.alphaNum)
    let ddashed = "--" ++ s
    readP_to_S optName ddashed === [(OptName ddashed LongType, "")]

prop_shortOpt :: Property
prop_shortOpt =
  property $ do
    s <- forAll (Gen.string (Range.singleton 1) Gen.alphaNum)
    let dashed = "-" ++ s
    readP_to_S optName dashed === [(OptName dashed ShortType, "")]

prop_oldOpt :: Property
prop_oldOpt =
  property $ do
    s <- forAll (Gen.string (Range.constant 2 10) Gen.alphaNum)
    let dashed = "-" ++ s
    readP_to_S optName dashed === [(OptName dashed OldType, "")]

prop_mergeRanges :: Property
prop_mergeRanges =
  property $ do
    let num = Gen.int (Range.constant 0 200)
    xs <- forAll $ Gen.list (Range.constant 0 300) num
    ys <- forAll $ Gen.list (Range.constant 0 300) num
    let (xRanges, yRanges) = Layout.makeRangePair (nubSort xs) (nubSort ys)
    mergeRangesSlow xRanges yRanges === Layout.mergeRange xRanges yRanges

-- | O(N^2): Only for testing purposes
mergeRangesSlow :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int, Int)]
mergeRangesSlow xs ys = [(x1, x2, y1, y2) | (x1, x2) <- xs, (y1, y2) <- ys, x1 <= y1 && y1 <= x2 && x2 <= y2]
