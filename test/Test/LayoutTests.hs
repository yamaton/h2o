{-# LANGUAGE OverloadedStrings #-}

module Test.LayoutTests (tests) where

import qualified Layout
import Test.Helpers (test_parseUsage)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
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
        convertTabsToSpaces 3 "\t\t\ta\tab\tabc\tkk" @?= "         a  ab abc   kk\n"
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
