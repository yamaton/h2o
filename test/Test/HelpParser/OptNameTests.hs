module Test.HelpParser.OptNameTests (tests) where

import HelpParser (optName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.ParserCombinators.ReadP (readP_to_S)
import Type (OptName (..), OptNameType (..))

tests :: TestTree
tests =
  testGroup
    "\n ============= Test optName ============= "
    [ testCase "optName (long)" $
        readP_to_S optName "--help" @?= [(OptName "--help" LongType, "")],
      testCase "optName (short)" $
        readP_to_S optName "-o" @?= [(OptName "-o" ShortType, "")],
      testCase "optName (old)" $
        readP_to_S optName "-azvhP" @?= [(OptName "-azvhP" OldType, "")],
      testCase "optName (double dash alone)" $
        readP_to_S optName "-- " @?= [(OptName "--" DoubleDashAlone, " ")],
      testCase "optName (single dash alone)" $
        readP_to_S optName "- " @?= [(OptName "-" SingleDashAlone, " ")]
    ]
