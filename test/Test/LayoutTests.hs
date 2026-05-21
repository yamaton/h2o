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
                Opt [OptName "--p-maxiterate" LongType] "INTEGER" "Specifies how many iterative refinement cycles are performed after the initial progressive alignment. By default, no iterative refinement is performed. [optional]",
                Opt [OptName "--o-alignment" LongType] "ARTIFACT" "The aligned sequences. [required]"
              ],
      testCase "parseBlockwise keeps QIIME right-aligned status annotations" $
        Layout.parseBlockwise qiimeStatusAnnotations
          @?= [ Opt [OptName "--input-path" LongType] "PATH" "Path to file or directory that should be imported. [required]",
                Opt [OptName "--output-path" LongType] "ARTIFACT" "Path where output artifact should be written. [required]",
                Opt [OptName "--threads" LongType] "INTEGER" "Number of worker threads. [default: 1]",
                Opt [OptName "--dry-run" LongType] "" "Show plan. [optional]"
              ],
      testCase "parseBlockwise skips wrapped QIIME Choices metadata before description" $
        Layout.parseBlockwise qiimeWrappedChoices
          @?= [ Opt [OptName "--p-p-adj-method" LongType] "TEXT" "Method to adjust p-values. [default: 'holm']",
                Opt [OptName "--p-prv-cut" LongType] "NUMBER" "A numerical fraction between 0-1. [default: 0.1]"
              ],
      testCase "parseBlockwise skips multi-line QIIME Choices metadata before default" $
        let parsed = Layout.parseBlockwise qiimeMultiLineChoices
         in map (`findOpt` parsed) ["--p-alpha-metrics", "--p-beta-metrics", "--p-random-seed"]
              @?= [ Just (Opt [OptName "--p-alpha-metrics" LongType] "TEXT..." "[default: ['pielou_e', 'observed_features', 'shannon']]"),
                    Just (Opt [OptName "--p-beta-metrics" LongType] "TEXT..." "[default: ['braycurtis', 'jaccard']]"),
                    Just (Opt [OptName "--p-random-seed" LongType] "INTEGER" "[optional]")
                  ],
      testCase "parseBlockwise ignores right-aligned status when estimating option-line offset" $
        let parsed = Layout.parseBlockwise qiimeCacheStore
         in findOpt "--key" parsed
              @?= Just (Opt [OptName "--key" LongType] "TEXT" "The key to save the artifact under (must be a valid Python identifier). [required]")
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
          "                                                                    [optional]",
          "Outputs:",
          "  --o-alignment ARTIFACT FeatureData[AlignedSequence]\185 |",
          "    FeatureData[AlignedProteinSequence]\178",
          "                          The aligned sequences.                    [required]"
        ]
    qiimeStatusAnnotations =
      unlines
        [ "Options:",
          "  --input-path PATH       Path to file or directory that should be imported.",
          "                                                                    [required]",
          "  --output-path ARTIFACT  Path where output artifact should be written.",
          "                                                                    [required]",
          "  --threads INTEGER       Number of worker threads.",
          "                                                                    [default: 1]",
          "  --dry-run               Show plan.",
          "                                                                    [optional]"
        ]
    qiimeWrappedChoices =
      unlines
        [ "Parameters:",
          "  --p-p-adj-method TEXT Choices('holm', 'hochberg', 'hommel', 'bonferroni',",
          "    'BH', 'BY', 'fdr', 'none')",
          "                         Method to adjust p-values.          [default: 'holm']",
          "  --p-prv-cut NUMBER     A numerical fraction between 0-1.  [default: 0.1]"
        ]
    qiimeMultiLineChoices =
      unlines
        [ "Parameters:",
          "  --p-sampling-depth INTEGER",
          "    Range(1, None)        The total number of observations that each sample",
          "                          in `table` should be resampled to. Samples where the",
          "                          total number of observations in `table` is less than",
          "                          `sampling-depth` will be not be included in the",
          "                          output tables.                            [required]",
          "  --m-metadata-file METADATA...",
          "    (multiple arguments   The sample metadata used in generating Emperor",
          "     will be merged)      plots.                                    [required]",
          "  --p-n INTEGER           The number of resampled tables that should be",
          "    Range(1, None)        generated.                                [required]",
          "  --p-replacement / --p-no-replacement",
          "                          Resample `table` with replacement (i.e., bootstrap)",
          "                          or without replacement (i.e., rarefaction).",
          "                                                                    [required]",
          "  --p-kmer-size INTEGER   Length of kmers to generate.           [default: 16]",
          "  --p-tfidf / --p-no-tfidf",
          "                          If True, kmers will be scored using TF-IDF and",
          "                          output frequencies will be weighted by scores. If",
          "                          False, kmers are counted without TF-IDF scores.",
          "                                                              [default: False]",
          "  --p-max-df VALUE Float % Range(0, 1, inclusive_end=True) | Int",
          "                          Ignore kmers that have a frequency strictly higher",
          "                          than the given threshold. If float, the parameter",
          "                          represents a proportion of sequences, if an integer",
          "                          it represents an absolute count.      [default: 1.0]",
          "  --p-min-df VALUE Float % Range(0, 1) | Int",
          "                          Ignore kmers that have a frequency strictly lower",
          "                          than the given threshold. If float, the parameter",
          "                          represents a proportion of sequences, if an integer",
          "                          it represents an absolute count.        [default: 1]",
          "  --p-max-features INTEGER",
          "                          If not None, build a vocabulary that only considers",
          "                          the top max-features ordered by frequency (or TF-IDF",
          "                          score).                                   [optional]",
          "  --p-alpha-average-method TEXT Choices('mean', 'median')",
          "                          Method to use for averaging alpha diversity.",
          "                                                           [default: 'median']",
          "  --p-beta-average-method TEXT Choices('non-metric-mean',",
          "    'non-metric-median', 'medoid')",
          "                          Method to use for averaging beta diversity.",
          "                                                           [default: 'medoid']",
          "  --p-pc-dimensions INTEGER",
          "                          Number of principal coordinate dimensions to",
          "                          present in the 2D scatterplot.          [default: 3]",
          "  --p-color-by TEXT       Categorical measure from the input Metadata that",
          "                          should be used for color-coding the 2D scatterplot.",
          "                                                                    [optional]",
          "  --p-norm TEXT Choices('None', 'l1', 'l2')",
          "                          Normalization procedure applied to TF-IDF scores.",
          "                          Ignored if tfidf=False. l2: Sum of squares of vector",
          "                          elements is 1. l1: Sum of absolute values of vector",
          "                          elements is 1.                     [default: 'None']",
          "  --p-alpha-metrics TEXT... Choices('ace', 'berger_parker_d',",
          "    'brillouin_d', 'chao1', 'chao1_ci', 'dominance', 'doubles', 'enspie',",
          "    'esty_ci', 'fisher_alpha', 'gini_index', 'goods_coverage', 'heip_e',",
          "    'kempton_taylor_q', 'lladser_pe', 'margalef', 'mcintosh_d', 'mcintosh_e',",
          "    'menhinick', 'michaelis_menten_fit', 'observed_features', 'osd',",
          "    'pielou_e', 'robbins', 'shannon', 'simpson', 'simpson_e', 'singles',",
          "    'strong')",
          "                       [default: ['pielou_e', 'observed_features', 'shannon']]",
          "  --p-beta-metrics TEXT... Choices('aitchison', 'braycurtis', 'canberra',",
          "    'canberra_adkins', 'chebyshev', 'cityblock', 'correlation', 'cosine',",
          "    'dice', 'euclidean', 'hamming', 'jaccard', 'jensenshannon', 'matching',",
          "    'minkowski', 'rogerstanimoto', 'russellrao', 'seuclidean', 'sokalsneath',",
          "    'sqeuclidean', 'yule')",
          "                                          [default: ['braycurtis', 'jaccard']]",
          "  --p-random-seed INTEGER                                           [optional]"
        ]
    qiimeCacheStore =
      unlines
        [ "Options:",
          "  --cache DIRECTORY     Path to an existing cache to save into.     [required]",
          "  --artifact-path FILE  Path to a .qza to save into the cache.      [required]",
          "  --key TEXT            The key to save the artifact under (must be a valid",
          "                        Python identifier).                         [required]",
          "  --help                Show this message and exit."
        ]
    findOpt raw opts =
      case [opt | opt@(Opt names _ _) <- opts, any (\name -> _raw name == raw) names] of
        opt : _ -> Just opt
        [] -> Nothing

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
