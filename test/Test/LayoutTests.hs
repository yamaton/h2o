{-# LANGUAGE OverloadedStrings #-}

module Test.LayoutTests (tests) where

import qualified Layout
import Test.Helpers (test_parseUsage)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Type (Opt (..), OptName (..), OptNameType (..))
import Utils (convertTabsToSpaces)

tests :: TestTree
tests =
  testGroup
    "Layout Tests"
    [ layoutTests
    , emptyInputTests
    , parseUsageTests
    ]

layoutTests :: TestTree
layoutTests =
  testGroup
    "Test layouts"
    [ -- convertTabsToSpaces inserts newline \n at the last.
      testCase "convertTabsToSpaces 1" $
        convertTabsToSpaces 4 "aa\tb\tccddddddddd\t" @?= "aa  b   ccddddddddd \n",
      testCase "convertTabsToSpaces 2" $
        convertTabsToSpaces 3 "\t\t\ta\tab\tabc\tkk" @?= "         a  ab abc   kk\n",
      testCase "parseBlockwise handles QIIME typed option continuations" $
        Layout.parseBlockwise qiimeTypedOptions
          @?= [ Opt [OptName "--i-sequences" LongType] "ARTIFACT" "The sequences to be aligned. [required]",
                Opt [OptName "--p-strategy" LongType] "TEXT" "Specifies the multiple alignment strategy to use. Exactly one strategy may be specified.",
                Opt [OptName "--p-maxiterate" LongType] "INTEGER" "Specifies how many iterative refinement cycles are performed after the initial progressive alignment. By default, no iterative refinement is performed.",
                Opt [OptName "--o-alignment" LongType] "ARTIFACT" "The aligned sequences. [required]"
              ]
    ]
  where
    qiimeTypedOptions =
      unlines
        [ "Inputs:",
          "  --i-sequences ARTIFACT FeatureData[Sequence]\185 |",
          "    FeatureData[ProteinSequence]\178",
          "                          The sequences to be aligned.              [required]",
          "Parameters:",
          "  --p-strategy TEXT Choices('auto', 'genafpair', 'globalpair', 'localpair',",
          "    'nofft')              Specifies the multiple alignment strategy to use.",
          "                          Exactly one strategy may be specified.",
          "  --p-maxiterate INTEGER  Specifies how many iterative refinement cycles are",
          "    Range(0, None)        performed after the initial progressive alignment.",
          "                          By default, no iterative refinement is performed.",
          "Outputs:",
          "  --o-alignment ARTIFACT FeatureData[AlignedSequence]\185 |",
          "    FeatureData[AlignedProteinSequence]\178",
          "                          The aligned sequences.                    [required]"
        ]

emptyInputTests :: TestTree
emptyInputTests =
  testGroup
    "Empty input handling"
    [ testCase "preprocessBlockwise on empty string" $
        Layout.preprocessBlockwise "" @?= [],
      testCase "parseBlockwise on empty string" $
        Layout.parseBlockwise "" @?= []
    ]

parseUsageTests :: TestTree
parseUsageTests =
  testGroup
    "\n ============= Test usage parser ============"
    [ test_parseUsage "  Usage:  cat [OPTION]... [FILE]...  \n" "cat [OPTION]... [FILE]...",
      test_parseUsage "SYNOPSIS\n    cat dog \nOTHER HEADER\n  baba\n" "cat dog",
      test_parseUsage "  Usage: \n    cat [OPTION]... [FILE]...\n    cat dog  \n  Other header:  keke\n\n" "cat [OPTION]... [FILE]...\ncat dog"
    ]
