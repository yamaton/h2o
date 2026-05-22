{-# LANGUAGE OverloadedStrings #-}

module Test.GoldenTests (tests) where

import CommandArgs (Config (..), ConfigOrVersion (..), Input (..), OutputFormat (..), defaultSubprocessBudget)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Io (getInputContent, pageToCommandSimple, run, toScript)
import System.FilePath (takeBaseName)
import System.IO (hClose, openBinaryTempFile)
import Test.Helpers (toLazyByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Printf (printf)

tests :: TestTree
tests =
  testGroup
    "Golden Tests"
    [ shellCompGoldenTests
    , integratedGoldenTestsCommandInput
    , integratedGoldenTestsFileInput
    , integratedGoldenTestsJsonInput
    , listSubcommandsTests
    , fileInputRobustnessTests
    ]

listSubcommandsTests :: TestTree
listSubcommandsTests =
  testGroup
    "--list-subcommands"
    [ testCase "honors depth 0" $ do
        assertListSubcommands 0 T.empty
    , testCase "honors depth 1" $ do
        assertListSubcommands 1 $
          T.unlines
            [ "init                      (Initialize a new project)"
            , "remote                    (Manage remote connections)"
            , "config                    (Get and set options)"
            ]
    , testCase "honors depth 2 with tree output" $ do
        assertListSubcommands 2 $
          T.unlines
            [ "init                      (Initialize a new project)"
            , "remote                    (Manage remote connections)"
            , "  add                       (Add a new remote)"
            , "  remove                    (Remove a remote)"
            , "  list                      (List all remotes)"
            , "config                    (Get and set options)"
            ]
    ]
  where
    assertListSubcommands depth expected = do
      actual <- runListSubcommands depth
      actual @?= expected

    runListSubcommands depth =
      run (C_ (Config (CommandInput "mockcmd" True) Native False True False depth defaultSubprocessBudget False))

fileInputRobustnessTests :: TestTree
fileInputRobustnessTests =
  testGroup
    "File input robustness"
    [ testCase "FileInput decodes invalid UTF-8 leniently" $ do
        (path, handle) <- openBinaryTempFile "/tmp" "h2o-invalid-utf8-test.txt"
        hClose handle
        BSL.writeFile path (BSL.pack "Options:\n  --help   Show \xff help\n")
        content <- getInputContent (FileInput path True)
        let text = T.pack content
        assertBool "option text should survive invalid bytes" (T.pack "--help" `T.isInfixOf` text)
        assertBool "invalid byte should be replaced, not dropped" (T.pack "\xfffd" `T.isInfixOf` text)
    ]

shellCompGoldenTests :: TestTree
shellCompGoldenTests =
  testGroup
    "Golden Tests of shell completions"
    [ goldenVsString
        "minimap2 fish"
        "test/golden/minimap2.fish"
        (actionFish "test/golden/minimap2.txt"),
      goldenVsString
        "minimap2 zsh"
        "test/golden/minimap2.zsh"
        (actionZsh "test/golden/minimap2.txt"),
      goldenVsString
        "minimap2 bash"
        "test/golden/minimap2.sh"
        (actionBash "test/golden/minimap2.txt"),
      --------------
      goldenVsString
        "bowtie2 fish"
        "test/golden/bowtie2.fish"
        (actionFish "test/golden/bowtie2.txt")
    ]
  where
    action shell x =
      toLazyByteString . toScript shell
        <$> (pageToCommandSimple (takeBaseName x) False =<< getInputContent (FileInput x True))
    actionFish = action Fish
    actionZsh = action Zsh
    actionBash = action Bash

integratedGoldenTestsCommandInput :: TestTree
integratedGoldenTestsCommandInput =
  testGroup
    "Integrated tests"
    (map toTestTree commands)
  where
    commands = ["h2o", "mockcmd"]
    conf name = C_ (Config (CommandInput name True) Native False False False 4 defaultSubprocessBudget False)
    runWithCommand name = toLazyByteString <$> run (conf name)
    toTestTree name =
      goldenVsString
        ("h2o --skip-man --command " ++ name)
        (printf "test/golden/%s.txt" name :: String)
        (runWithCommand name)

integratedGoldenTestsFileInput :: TestTree
integratedGoldenTestsFileInput =
  testGroup
    "Integrated tests"
    (map toTestTree triples)
  where
    -- [note] bcftools-mpileup is marginal and parsing give incomplete results
    --        such marginal example is a good at catching unexpected behviors.
    commandNames =
      [ "rsync",
        "grep",
        "bcftools-stats",
        "bcftools-mpileup",
        "snakemake",
        "iqtree"
      ] ::
        [String]
    inputFiles = [printf "test/golden/%s-input.txt" name | name <- commandNames]
    outputFiles = [printf "test/golden/%s.txt" name | name <- commandNames]
    triples = zip3 commandNames inputFiles outputFiles

    conf filepath = C_ (Config (FileInput filepath True) Native False False False 4 defaultSubprocessBudget False)
    runWithCommand filepath = toLazyByteString <$> run (conf filepath)
    toTestTree (_, inputFile, outputFile) =
      goldenVsString
        ("h2o --file " ++ inputFile)
        outputFile
        (runWithCommand inputFile)

integratedGoldenTestsJsonInput :: TestTree
integratedGoldenTestsJsonInput =
  testGroup
    "Integrated tests"
    (map toTestTree triples)
  where
    -- [note] bcftools-mpileup is marginal and parsing give incomplete results
    --        such marginal example is a good at catching unexpected behviors.
    commandNames =
      [ "h2o",
        "stack"
      ] ::
        [String]
    inputFiles = [printf "test/golden/%s.json" name | name <- commandNames]
    outputFiles = [printf "test/golden/%s-output.txt" name | name <- commandNames]
    triples = zip3 commandNames inputFiles outputFiles

    conf filepath = C_ (Config (JsonInput filepath) Native False False False 4 defaultSubprocessBudget False)
    runWithCommand filepath = toLazyByteString <$> run (conf filepath)
    toTestTree (_, inputFile, outputFile) =
      goldenVsString
        ("h2o --loadjson " ++ inputFile)
        outputFile
        (runWithCommand inputFile)
