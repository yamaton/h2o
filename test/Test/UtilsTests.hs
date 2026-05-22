{-# LANGUAGE OverloadedStrings #-}

module Test.UtilsTests (tests) where

import qualified Control.Exception
import Control.Exception (try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as List
import qualified Data.Text as T
import qualified GenFishCompletions as GenFish
import qualified H2OError
import H2OError (H2OError (..))
import qualified Postprocess
import Subcommand (firstTwoWordsLoc, parseSubcommand)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import qualified Type
import Type (Opt (..), OptName (..), OptNameType (..))
import qualified Utils
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
      testCase "parseSubcommand joins wrapped descriptions" $
        parseSubcommand
          "Commands:\n\
          \  composition         Plugin for compositional data analysis.\n\
          \  cutadapt            Plugin for removing adapter sequences, primers, and\n\
          \                      other unwanted sequence from sequence data.\n\
          \  sample-classifier   Plugin for machine learning prediction of sample\n\
          \                      metadata.\n\
          \  stats               Plugin for statistical analyses.\n"
          @?= [ Type.Subcommand "composition" [] "Plugin for compositional data analysis.",
                Type.Subcommand "cutadapt" [] "Plugin for removing adapter sequences, primers, and other unwanted sequence from sequence data.",
                Type.Subcommand "sample-classifier" [] "Plugin for machine learning prediction of sample metadata.",
                Type.Subcommand "stats" [] "Plugin for statistical analyses."
              ],
      testCase "parseSubcommand keeps comma-separated aliases" $
        parseSubcommand
          "SUBCOMMANDS:\n\
          \  shell                       Generate shell init scripts\n\
          \  remove, uninstall           Remove packages from active environment\n\
          \  search                      Find packages in active environment or channels\n\
          \                              This is equivalent to `repoquery search` command\n"
          @?= [ Type.Subcommand "shell" [] "Generate shell init scripts",
                Type.Subcommand "remove" ["uninstall"] "Remove packages from active environment",
                Type.Subcommand "search" [] "Find packages in active environment or channels This is equivalent to `repoquery search` command"
              ],
      testCase "parseSubcommand accepts Cobra colon command sections" $
        parseSubcommand
          "AVAILABLE COMMANDS\n\
          \  login:       Log in to a GitHub account\n\
          \  setup-git:   Setup git with GitHub CLI\n"
          @?= [ Type.Subcommand "login" [] "Log in to a GitHub account",
                Type.Subcommand "setup-git" [] "Setup git with GitHub CLI"
              ],
      testCase "parseSubcommand ignores Cobra help topics" $
        parseSubcommand
          "CORE COMMANDS\n\
          \  auth:        Authenticate gh and git with GitHub\n\
          \  repo:        Manage repositories\n\
          \HELP TOPICS\n\
          \  actions:     Learn about working with GitHub Actions\n\
          \  reference:   A comprehensive reference of all gh commands\n"
          @?= [ Type.Subcommand "auth" [] "Authenticate gh and git with GitHub",
                Type.Subcommand "repo" [] "Manage repositories"
              ],
      testCase "parseSubcommand ignores colon rows outside command sections" $
        parseSubcommand
          "Notes:\n\
          \  Note:  This is not a command\n\
          \  Type:  statistics\n"
          @?= [],
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
      testCase "smartUnwords removes soft hyphenation" $
        Utils.smartUnwords ["git-", "status"] @?= "gitstatus",
      testCase "smartUnwords preserves wrapped option fragments" $
        Utils.smartUnwords ["Use with --no-", "indent or --compact"] @?= "Use with --no-indent or --compact",
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
      testCase "hasErrorMessageAtTop: ordinary missing text is not an error" $
        Utils.hasErrorMessageAtTop "demo" "Show missing dependency information\n  --help   Show help" @?= False,
      testCase "hasErrorMessageAtTop: missing argument is an error" $
        Utils.hasErrorMessageAtTop "demo" "missing argument: FILE" @?= True,
      testCase "hasErrorMessageAtTop: prefixed error is an error" $
        Utils.hasErrorMessageAtTop "demo" "error: unknown option --foo" @?= True,
      testCase "hasErrorMessageAtTop: usage is not an error" $
        Utils.hasErrorMessageAtTop "demo" "Usage: demo [OPTIONS]" @?= False,
      testCase "hasErrorMessageAtTop: command-prefixed option error is an error" $
        Utils.hasErrorMessageAtTop "demo" "demo: unrecognized option '--bad'" @?= True,
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
      -- decodeUtf8Lenient: the strict TLE.decodeUtf8 previously used in
      -- IoHelper crashed h2o on any non-UTF-8 byte produced by a command
      -- (Latin-1 locales, binary stderr, older macOS man pages). The
      -- lenient variant must (1) not raise, and (2) preserve surrounding
      -- valid bytes so the parser still sees real option text.
      testCase "decodeUtf8Lenient: valid ASCII is unchanged" $
        Utils.decodeUtf8Lenient (BSL.pack "hello") @?= "hello",
      testCase "decodeUtf8Lenient: valid multibyte UTF-8 decodes correctly" $
        -- 0xE3 0x81 0x82 is the UTF-8 encoding of U+3042 (HIRAGANA A).
        Utils.decodeUtf8Lenient (BSL.pack "\xE3\x81\x82") @?= "\x3042",
      testCase "decodeUtf8Lenient: invalid bytes do not raise; surroundings kept" $
        let txt = Utils.decodeUtf8Lenient (BSL.pack "hello\xFFworld\xFE!")
         in do
              assertBool "prefix preserved" (T.isInfixOf "hello" txt)
              assertBool "middle preserved" (T.isInfixOf "world" txt)
              assertBool "suffix preserved" (T.isInfixOf "!" txt),
      testCase "decodeUtf8Lenient: empty input is empty" $
        Utils.decodeUtf8Lenient (BSL.pack "") @?= "",
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
              Right (Opt names _ _) -> map _raw names @?= ["--help"],
      testCase "FromJSON Opt rejects empty names array" $
        let bad = BSL.pack "{\"names\":[],\"argument\":\"\",\"description\":\"d\"}"
         in case Aeson.eitherDecode bad :: Either String Opt of
              Right _ -> assertFailure "expected parse failure for empty names"
              Left err ->
                assertBool ("error mentions non-empty requirement: " ++ err)
                  ("non-empty" `List.isInfixOf` err),
      testCase "FromJSON Command accepts missing aliases as empty" $
        let json = BSL.pack "{\"name\":\"demo\",\"description\":\"d\",\"options\":[]}"
         in (Aeson.eitherDecode json :: Either String Type.Command)
              @?= Right (Type.Command "demo" [] "d" "" [] [] ""),
      testCase "ToJSON Command includes non-empty aliases" $
        let cmd = Type.Command "remove" ["uninstall"] "Remove packages" "" [] [] ""
            encoded = BSL.unpack (Aeson.encode cmd)
         in do
              assertBool encoded ("\"aliases\"" `List.isInfixOf` encoded)
              assertBool encoded ("\"uninstall\"" `List.isInfixOf` encoded),
      testCase "ToJSON Command omits empty aliases" $
        let cmd = Type.Command "demo" [] "d" "" [] [] ""
            encoded = BSL.unpack (Aeson.encode cmd)
         in assertBool encoded (not $ "\"aliases\"" `List.isInfixOf` encoded),
      testCase "topTenPercentile returns Nothing for empty list" $
        Utils.topTenPercentile ([] :: [Int]) @?= Nothing,
      testCase "topTenPercentile returns Just for non-empty list" $
        Utils.topTenPercentile [1 .. 10 :: Int] @?= Just 9,
      -- H2OError: error-reporting types replaced the previous `die` calls
      -- so library callers can `try`/`catch` instead of having the whole
      -- process exit. Verify rendering and that the exception is catchable.
      testCase "renderH2OError produces user-friendly NoHelpOrMan message" $
        H2OError.renderH2OError (NoHelpOrMan "foo")
          @?= "Error: No help or man page found for 'foo'. Is the command installed?",
      testCase "renderH2OError preserves aeson error in JsonDecodeFailed" $
        H2OError.renderH2OError (JsonDecodeFailed "x.json" "Error in $: oops")
          @?= "Error: Cannot decode JSON from 'x.json'. Ensure the file contains a valid Command schema.\n  Error in $: oops",
      testCase "renderH2OError reports subprocess budget exhaustion as incomplete output" $
        H2OError.renderH2OError (SubprocessBudgetExhausted "qiime kmerizer" 500)
          @?= "Error: Subprocess budget exhausted while scanning 'qiime kmerizer'. h2o stopped to avoid emitting incomplete output. The current limit is 500 help/man invocations; try a lower --depth value or a higher --subprocess-budget value.",
      testCase "H2OError is catchable via Control.Exception.try" $ do
        result <-
          try (Control.Exception.throwIO (NoExtractableOptions "demo")) ::
            IO (Either H2OError ())
        case result of
          Left (NoExtractableOptions name) -> name @?= "demo"
          Left other ->
            assertFailure ("unexpected H2OError variant: " ++ show other)
          Right _ -> assertFailure "exception was not raised"
    ]
