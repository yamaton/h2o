{-# LANGUAGE OverloadedStrings #-}

module Test.UtilsTests (tests) where

import qualified GenFishCompletions as GenFish
import qualified Postprocess
import Subcommand (firstTwoWordsLoc)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Type (Opt (..), OptName (..), OptNameType (..))
import Utils (getMostFrequent, isUsageBlock, splitsAt, groupConsecutive, toRanges, removeBackspaceOverstrikes, stripAnsiEscapes)

tests :: TestTree
tests =
  testGroup
    "\n ============ misc =============="
    [ testCase "firstTwoWordsLoc: empty" $
        firstTwoWordsLoc
          "  "
          @?= (-1, -1),
      testCase "firstTwoWordsLoc: single word" $
        firstTwoWordsLoc
          " hi"
          @?= (1, -1),
      testCase "firstTwoWordsLoc 0" $
        firstTwoWordsLoc
          "  stop        Stop one or more running containers   "
          @?= (2, 14),
      testCase "firstTwoWordsLoc 1" $
        firstTwoWordsLoc
          "stop        Stop one or more running containers   "
          @?= (0, 12),
      testCase "firstTwoWordsLoc 2" $
        firstTwoWordsLoc
          "   hi               there   "
          @?= (3, 20),
      testCase "getMostFrequent [1, -4, 2, 9, 1, -4, -3, 7, -4, -4, 1] == Just (-4)" $
        getMostFrequent [1 :: Int, -4, 2, 9, 1, -4, -3, 7, -4, -4, 1] @?= Just (-4 :: Int),
      testCase "groupConsecutive [2, 3, 4, 8, 10, 11] == [[2, 3, 4], [8], [10, 11]]" $
        groupConsecutive [2, 3, 4, 8, 10, 11] @?= [[2, 3, 4], [8], [10, 11]],
      testCase "toRanges [2, 3, 4, 8, 10, 11] == [(2, 5), (8, 9), (10, 12)]" $
        toRanges [2, 3, 4, 8, 10, 11] @?= [(2, 5), (8, 9), (10, 12)],
      testCase "splitsAt \"0123456789\" [0, 3, 5] == [\"012\", \"34\", \"56789\"]" $
        splitsAt "0123456789" [0, 3, 5] @?= ["012", "34", "56789"],
      testCase "splitsAt \"0123456789\" [0] == [\"0123456789\"]" $
        splitsAt "0123456789" [0] @?= ["0123456789"],
      testCase "splitsAt \"0123456789\" [] == [\"0123456789\"]" $
        splitsAt "0123456789" [] @?= ["0123456789"],
      testCase "truncateAfterPeriod 1" $
        GenFish.truncateAfterPeriod "hello, i.e. good bye!" @?= "hello, i.e. good bye!",
      testCase "truncateAfterPeriod 2" $
        GenFish.truncateAfterPeriod "baba. keke" @?= "baba.",
      testCase "truncateAfterPeriod 3" $
        GenFish.truncateAfterPeriod "baba... keke" @?= "baba...",
      testCase "truncateAfterPeriod 4" $
        GenFish.truncateAfterPeriod "baba .. keke" @?= "baba ..",
      testCase "fixOpt 1" $
        Postprocess.fixShortOptWithArgWithoutSpace
          (Opt [OptName "-Ttagsfile" OldType, OptName "--tag-file" LongType] "tagsfile" "Specifies a tags file...")
          @?= Opt [OptName "-T" ShortType, OptName "--tag-file" LongType] "tagsfile" "Specifies a tags file...",
      testCase "isUsageBlock" $
        Utils.isUsageBlock "Usage: rsem-bam2readdepth sorted_bam_input readdepth_output" @?= True,
      testCase "isUsageBlock" $
        Utils.isUsageBlock "SYNOPSIS\n      rsem-bam2readdepth sorted_bam_input readdepth_output" @?= True,
      -- removeBackspaceOverstrikes
      testCase "removeBackspaceOverstrikes: bold via overstrike" $
        removeBackspaceOverstrikes ("H\x08" <> "He\x08" <> "el\x08" <> "lp\x08" <> "p") @?= "Help",
      testCase "removeBackspaceOverstrikes: no backspaces" $
        removeBackspaceOverstrikes "Hello" @?= "Hello",
      testCase "removeBackspaceOverstrikes: empty" $
        removeBackspaceOverstrikes "" @?= "",
      testCase "removeBackspaceOverstrikes: backspace at start" $
        removeBackspaceOverstrikes ("\x08" <> "abc") @?= "abc",
      testCase "removeBackspaceOverstrikes: consecutive backspaces" $
        removeBackspaceOverstrikes ("ab\x08\x08" <> "cd") @?= "cd",
      -- stripAnsiEscapes
      testCase "stripAnsiEscapes: color code" $
        stripAnsiEscapes "\x1B[31mred\x1B[0m" @?= "red",
      testCase "stripAnsiEscapes: bold + color" $
        stripAnsiEscapes "\x1B[1;32mbold green\x1B[0m" @?= "bold green",
      testCase "stripAnsiEscapes: no escapes" $
        stripAnsiEscapes "plain text" @?= "plain text",
      testCase "stripAnsiEscapes: erase line" $
        stripAnsiEscapes "text\x1B[2Kmore" @?= "textmore",
      testCase "stripAnsiEscapes: empty" $
        stripAnsiEscapes "" @?= "",
      testCase "stripAnsiEscapes: bare ESC" $
        stripAnsiEscapes "\x1Btext" @?= "text"
    ]
