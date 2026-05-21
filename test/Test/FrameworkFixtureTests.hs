module Test.FrameworkFixtureTests (tests) where

import Layout (parseBlockwise)
import Subcommand (parseSubcommand)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Type (Opt (..), OptName (..), Subcommand (..))

tests :: TestTree
tests =
  testGroup
    "CLI framework fixtures"
    [ cobraFixtures
    , argparseFixtures
    , clapFixtures
    ]

cobraFixtures :: TestTree
cobraFixtures =
  testGroup
    "Cobra"
    [ testCase "gh root help parses colon subcommand rows but ignores help topics" $ do
        content <- readFile "test/fixtures/frameworks/cobra-gh-help.txt"
        parseSubcommand content
          @?= [ Subcommand "auth" [] "Authenticate gh and git with GitHub"
              , Subcommand "browse" [] "Open the repository in the browser"
              , Subcommand "project" [] "Work with GitHub Projects."
              , Subcommand "repo" [] "Manage repositories"
              , Subcommand "cache" [] "Manage GitHub Actions caches"
              , Subcommand "run" [] "View details about workflow runs"
              , Subcommand "workflow" [] "View details about GitHub Actions workflows"
              , Subcommand "co" [] "Alias for \"pr checkout\""
              , Subcommand "alias" [] "Create command shortcuts"
              , Subcommand "api" [] "Make an authenticated GitHub API request"
              , Subcommand "completion" [] "Generate shell completion scripts"
              ]
    , testCase "gh nested help parses AVAILABLE COMMANDS colon rows" $ do
        content <- readFile "test/fixtures/frameworks/cobra-gh-auth-help.txt"
        parseSubcommand content
          @?= [ Subcommand "login" [] "Log in to a GitHub account"
              , Subcommand "logout" [] "Log out of a GitHub account"
              , Subcommand "refresh" [] "Refresh stored authentication credentials"
              , Subcommand "setup-git" [] "Setup git with GitHub CLI"
              , Subcommand "status" [] "Display active account and authentication state on each known GitHub host"
              , Subcommand "switch" [] "Switch active GitHub account"
              , Subcommand "token" [] "Print the authentication token gh uses for a hostname and account"
              ]
    ]

argparseFixtures :: TestTree
argparseFixtures =
  testGroup
    "argparse"
    [ testCase "python json.tool parses wrapped option descriptions" $ do
        opts <- parseBlockwise <$> readFile "test/fixtures/frameworks/argparse-json-tool-help.txt"
        assertOpt
          opts
          "--json-lines"
          ["--json-lines"]
          ""
          "parse input using the JSON Lines format. Use with --no-indent or --compact to produce valid JSON Lines output."
        assertOpt
          opts
          "--indent"
          ["--indent"]
          "INDENT"
          "separate items with newlines and use this number of spaces for indentation"
        assertOpt
          opts
          "--compact"
          ["--compact"]
          ""
          "suppress all whitespace separation (most compact)"
    ]

clapFixtures :: TestTree
clapFixtures =
  testGroup
    "clap"
    [ testCase "uv help parses Commands section and hanging option descriptions" $ do
        content <- readFile "test/fixtures/frameworks/clap-uv-help.txt"
        parseSubcommand content
          @?= [ Subcommand "auth" [] "Manage authentication"
              , Subcommand "run" [] "Run a command or script"
              , Subcommand "init" [] "Create a new project"
              , Subcommand "tool" [] "Run and install commands provided by Python packages"
              , Subcommand "python" [] "Manage Python versions and installations"
              , Subcommand "pip" [] "Manage Python packages with a pip-compatible interface"
              , Subcommand "help" [] "Display documentation for a command"
              ]
        let opts = parseBlockwise content
        assertOpt
          opts
          "--no-cache"
          ["-n", "--no-cache"]
          ""
          "Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation [env: UV_NO_CACHE=]"
        assertOpt
          opts
          "--color"
          ["--color"]
          "<COLOR_CHOICE>"
          "Control the use of color in output [possible values: auto, always, never]"
        assertOpt
          opts
          "--quiet"
          ["-q", "--quiet"]
          ""
          "Do not print any output"
        assertOpt
          opts
          "--verbose"
          ["-v", "--verbose"]
          ""
          "Use verbose output"
    , testCase "cargo help parses comma-separated command aliases" $ do
        content <- readFile "test/fixtures/frameworks/clap-cargo-help.txt"
        parseSubcommand content
          @?= [ Subcommand "build" ["b"] "Compile the current package"
              , Subcommand "check" ["c"] "Analyze the current package and report errors, but don't build object files"
              , Subcommand "clean" [] "Remove the target directory"
              , Subcommand "doc" ["d"] "Build this package's and its dependencies' documentation"
              , Subcommand "run" ["r"] "Run a binary or example of the local package"
              , Subcommand "test" ["t"] "Run the tests"
              , Subcommand "help" [] "Display documentation for a command"
              ]
        assertOpt
          (parseBlockwise content)
          "--color"
          ["--color"]
          "<WHEN>"
          "Coloring [possible values: auto, always, never]"
    ]

assertOpt :: [Opt] -> String -> [String] -> String -> String -> IO ()
assertOpt opts target expectedNames expectedArg expectedDesc =
  withOpt opts target $ \opt -> do
    optNames opt @?= expectedNames
    optArg opt @?= expectedArg
    optDesc opt @?= expectedDesc

withOpt :: [Opt] -> String -> (Opt -> IO ()) -> IO ()
withOpt opts target action =
  case [opt | opt <- opts, target `elem` optNames opt] of
    [] -> assertFailure $ "missing option: " ++ target
    opt : _ -> action opt

optNames :: Opt -> [String]
optNames (Opt names _ _) = [raw | OptName raw _ <- names]

optArg :: Opt -> String
optArg (Opt _ arg _) = arg

optDesc :: Opt -> String
optDesc (Opt _ _ desc) = desc
