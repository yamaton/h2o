{-# LANGUAGE OverloadedStrings #-}

module Test.UtilsTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as List
import qualified GenFishCompletions as GenFish
import qualified Postprocess
import Subcommand (firstTwoWordsLoc)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import qualified Type
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
      -- fixDuplicateOpts: drop strict losers regardless of absolute score.
      -- The previous `score_ < 1` cap let any inferior alternative scoring at
      -- least 1 stay, so users saw both the chosen winner and a near-duplicate
      -- in their completion script.
      testCase "fixDuplicateOpts: inferior duplicate is discarded even when its score >= 1" $
        let -- "Output file path." -> descScore +1 (ends with '.'), argScore +2 ("FILE"
            -- single uppercase word), nameScore +1 -> total 4.
            better =
              Opt
                [OptName "--out" LongType]
                "FILE"
                "Output file path."
            -- "output file" -> descScore -1 (lowercase first char),
            -- argScore +2, nameScore +1 -> total 2 (>= 1, so old code kept it).
            worse =
              Opt
                [OptName "--out" LongType]
                "FILE"
                "output file"
            result = Postprocess.fixDuplicateOpts [worse, better]
         in result @?= [better],
      testCase "fixDuplicateOpts: tied-best duplicates are both kept" $
        let a =
              Opt
                [OptName "--out" LongType]
                "FILE"
                "Output file path."
            b =
              Opt
                [OptName "--out" LongType]
                "DIR"
                "Output directory path."
         in -- Both score 4: both kept (no clear winner to discard).
            length (Postprocess.fixDuplicateOpts [a, b]) @?= 2,
      -- Two different options that share a single short name (rsync-style
      -- `-h` belonging to both --help and --human-readable) must not be
      -- collapsed even when one outscores the other; the dedup is gated on
      -- the *full* name list matching, not on any individual name overlap.
      testCase "fixDuplicateOpts: alternatives that share a short name are kept" $
        let helpOpt =
              Opt
                [OptName "--help" LongType, OptName "-h" ShortType]
                "(*)"
                "Show help (* -h is help only on its own)."
            humanOpt =
              Opt
                [OptName "--human-readable" LongType, OptName "-h" ShortType]
                ""
                "output sizes in human-readable format"
         in length (Postprocess.fixDuplicateOpts [helpOpt, humanOpt]) @?= 2,
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
        stripAnsiEscapes "\x1Btext" @?= "text",
      -- toOptionNameType: classification, with Nothing for non-dash input
      -- (replaces the previous `error` that crashed the whole program when
      -- invalid JSON was loaded via --loadjson).
      testCase "toOptionNameType: long" $
        Type.toOptionNameType "--help" @?= Just LongType,
      testCase "toOptionNameType: short" $
        Type.toOptionNameType "-h" @?= Just ShortType,
      testCase "toOptionNameType: old" $
        Type.toOptionNameType "-help" @?= Just OldType,
      testCase "toOptionNameType: single dash" $
        Type.toOptionNameType "-" @?= Just SingleDashOnlyType,
      testCase "toOptionNameType: double dash" $
        Type.toOptionNameType "--" @?= Just DoubleDashOnlyType,
      testCase "toOptionNameType: invalid -> Nothing" $
        Type.toOptionNameType "foo" @?= Nothing,
      -- FromJSON Opt: invalid option name should fail through aeson rather
      -- than crash via `error`. The error message must point at what's wrong
      -- so the user can fix the JSON.
      testCase "FromJSON Opt rejects non-dash name with informative error" $
        let bad = BSL.pack "{\"names\":[\"foo\"],\"argument\":\"\",\"description\":\"d\"}"
         in case Aeson.eitherDecode bad :: Either String Opt of
              Right _ -> assertFailure "expected JSON parse to fail for invalid optname"
              Left err -> do
                assertBool ("error mentions 'Invalid option name': " ++ err)
                  ("Invalid option name" `List.isInfixOf` err)
                assertBool ("error mentions the offending value: " ++ err)
                  ("foo" `List.isInfixOf` err),
      testCase "FromJSON Opt accepts a valid option" $
        let good = BSL.pack "{\"names\":[\"--help\"],\"argument\":\"\",\"description\":\"d\"}"
         in case Aeson.eitherDecode good :: Either String Opt of
              Left err -> assertFailure ("expected parse to succeed: " ++ err)
              Right (Opt names _ _) -> map _raw names @?= ["--help"]
    ]
