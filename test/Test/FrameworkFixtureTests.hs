module Test.FrameworkFixtureTests (tests) where

import qualified Data.List as List
import qualified Data.Text as T
import GenJSON (toJSONText)
import Layout (parseBlockwise, parseUsage)
import Subcommand (parseSubcommand)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Type (Command (..), Opt (..), OptName (..), Subcommand (..))

tests :: TestTree
tests =
  testGroup
    "CLI framework fixtures"
    [ cobraFixtures
    , argparseFixtures
    , clapFixtures
    , expectedNameCoverage
    , fullSnapshotCoverage
    , jsonSmokeTests
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
    , testCase "uv wrapped env annotation keeps continuation text" $ do
        opts <- parseBlockwise <$> readFile "test/fixtures/frameworks/wrapped/uv-0.11.15-help-columns-80.txt"
        assertOpt
          opts
          "--no-python-downloads"
          ["--no-python-downloads"]
          ""
          "Disable automatic downloads of Python. [env: \"UV_PYTHON_DOWNLOADS=never\"]"
        assertOpt
          opts
          "--system-certs"
          ["--system-certs"]
          ""
          "Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]"
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
    , testCase "fd help parses clap repeated flags and repeated command arguments" $ do
        opts <- parseBlockwise <$> readFile "test/fixtures/frameworks/clap-fd-help.txt"
        assertOpt
          opts
          "--unrestricted"
          ["-u", "--unrestricted"]
          ""
          "Perform an unrestricted search, including ignored and hidden files. This is an alias for '--no-ignore --hidden'."
        assertOpt
          opts
          "--exec"
          ["-x", "--exec"]
          "<cmd>..."
          "Execute a command for each search result in parallel (use --threads=1 for sequential command execution). There is no guarantee of the order commands are executed in, and the order should not be depended upon. All positional arguments following --exec are considered to be arguments to the command - not to fd. It is therefore recommended to place the '-x'/'--exec' option last."
        assertOpt
          opts
          "--exec-batch"
          ["-X", "--exec-batch"]
          "<cmd>..."
          "Execute the given command once, with all search results as arguments. The order of the arguments is non-deterministic, and should not be relied upon."
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

expectedNameCoverage :: TestTree
expectedNameCoverage =
  testGroup
    "expected name coverage"
    [ assertExpectedSubcommands
        "gh root subcommands"
        "test/fixtures/frameworks/cobra-gh-help.txt"
        "test/fixtures/frameworks/expected/cobra-gh-subcommands.txt"
    , assertExpectedOptions
        "gh root options"
        "test/fixtures/frameworks/cobra-gh-help.txt"
        "test/fixtures/frameworks/expected/cobra-gh-options.txt"
    , assertExpectedSubcommands
        "gh auth subcommands"
        "test/fixtures/frameworks/cobra-gh-auth-help.txt"
        "test/fixtures/frameworks/expected/cobra-gh-auth-subcommands.txt"
    , assertExpectedOptions
        "gh auth options"
        "test/fixtures/frameworks/cobra-gh-auth-help.txt"
        "test/fixtures/frameworks/expected/cobra-gh-auth-options.txt"
    , assertExpectedSubcommands
        "uv subcommands"
        "test/fixtures/frameworks/clap-uv-help.txt"
        "test/fixtures/frameworks/expected/clap-uv-subcommands.txt"
    , assertExpectedOptions
        "uv options"
        "test/fixtures/frameworks/clap-uv-help.txt"
        "test/fixtures/frameworks/expected/clap-uv-options.txt"
    , assertExpectedSubcommands
        "cargo subcommands and aliases"
        "test/fixtures/frameworks/clap-cargo-help.txt"
        "test/fixtures/frameworks/expected/clap-cargo-subcommands.txt"
    , assertExpectedOptions
        "cargo options"
        "test/fixtures/frameworks/clap-cargo-help.txt"
        "test/fixtures/frameworks/expected/clap-cargo-options.txt"
    , assertExpectedOptions
        "fd options"
        "test/fixtures/frameworks/clap-fd-help.txt"
        "test/fixtures/frameworks/expected/clap-fd-options.txt"
    ]

assertExpectedOptions :: String -> FilePath -> FilePath -> TestTree
assertExpectedOptions label helpPath expectedPath =
  testCase label $ do
    content <- readFile helpPath
    expected <- readExpectedNames expectedPath
    let actual = List.nub . concatMap optNames $ parseBlockwise content
    assertContainsAll label expected actual

assertExpectedSubcommands :: String -> FilePath -> FilePath -> TestTree
assertExpectedSubcommands label helpPath expectedPath =
  testCase label $ do
    content <- readFile helpPath
    expected <- readExpectedNames expectedPath
    let actual = List.nub . concatMap subcommandNames $ parseSubcommand content
    assertContainsAll label expected actual

readExpectedNames :: FilePath -> IO [String]
readExpectedNames path =
  filter usefulLine . lines <$> readFile path
  where
    usefulLine "" = False
    usefulLine ('#' : _) = False
    usefulLine _ = True

assertContainsAll :: String -> [String] -> [String] -> IO ()
assertContainsAll label expected actual =
  case expected List.\\ actual of
    [] -> pure ()
    missing -> assertFailure $ label ++ " missing expected names: " ++ show missing

subcommandNames :: Subcommand -> [String]
subcommandNames (Subcommand name aliases _) = name : aliases

fullSnapshotCoverage :: TestTree
fullSnapshotCoverage =
  testGroup
    "full snapshot coverage"
    [ assertExpectedSubcommands
        "gh full subcommands"
        "test/fixtures/frameworks/full/gh-2.63.1-help.txt"
        "test/fixtures/frameworks/expected/full-gh-subcommands.txt"
    , assertExpectedOptions
        "gh full options"
        "test/fixtures/frameworks/full/gh-2.63.1-help.txt"
        "test/fixtures/frameworks/expected/full-gh-options.txt"
    , assertExpectedSubcommands
        "gh auth full subcommands"
        "test/fixtures/frameworks/full/gh-2.63.1-auth-help.txt"
        "test/fixtures/frameworks/expected/full-gh-auth-subcommands.txt"
    , assertExpectedOptions
        "gh auth full options"
        "test/fixtures/frameworks/full/gh-2.63.1-auth-help.txt"
        "test/fixtures/frameworks/expected/full-gh-auth-options.txt"
    , assertExpectedSubcommands
        "uv full subcommands"
        "test/fixtures/frameworks/full/uv-0.11.15-help.txt"
        "test/fixtures/frameworks/expected/full-uv-subcommands.txt"
    , assertExpectedOptions
        "uv full options"
        "test/fixtures/frameworks/full/uv-0.11.15-help.txt"
        "test/fixtures/frameworks/expected/full-uv-options.txt"
    , assertExpectedSubcommands
        "uv tool full subcommands"
        "test/fixtures/frameworks/full/uv-0.11.15-tool-help.txt"
        "test/fixtures/frameworks/expected/full-uv-tool-subcommands.txt"
    , assertExpectedOptions
        "uv tool full options"
        "test/fixtures/frameworks/full/uv-0.11.15-tool-help.txt"
        "test/fixtures/frameworks/expected/full-uv-tool-options.txt"
    , assertExpectedSubcommands
        "cargo full subcommands and aliases"
        "test/fixtures/frameworks/full/cargo-1.95.0-help.txt"
        "test/fixtures/frameworks/expected/full-cargo-subcommands.txt"
    , assertExpectedOptions
        "cargo full options"
        "test/fixtures/frameworks/full/cargo-1.95.0-help.txt"
        "test/fixtures/frameworks/expected/full-cargo-options.txt"
    , assertExpectedOptions
        "fd full options"
        "test/fixtures/frameworks/full/fd-10.4.2-help.txt"
        "test/fixtures/frameworks/expected/full-fd-options.txt"
    ]

jsonSmokeTests :: TestTree
jsonSmokeTests =
  testGroup
    "JSON smoke"
    [ testCase "gh full snapshot emits subcommands" $ do
        json <- snapshotJSON "gh" <$> readFile "test/fixtures/frameworks/full/gh-2.63.1-help.txt"
        assertJsonContains "gh subcommands field" "\"subcommands\"" json
        assertJsonContains "gh auth subcommand" "\"name\":\"auth\"" json
        assertJsonContains "gh api subcommand" "\"name\":\"api\"" json
    , testCase "uv full snapshot emits options and subcommands" $ do
        json <- snapshotJSON "uv" <$> readFile "test/fixtures/frameworks/full/uv-0.11.15-help.txt"
        assertJsonContains "uv subcommands field" "\"subcommands\"" json
        assertJsonContains "uv tool subcommand" "\"name\":\"tool\"" json
        assertJsonContains "uv no-python-downloads option" "\"--no-python-downloads\"" json
        assertJsonContains "uv env annotation" "UV_PYTHON_DOWNLOADS=never" json
    , testCase "cargo full snapshot emits subcommand aliases" $ do
        json <- snapshotJSON "cargo" <$> readFile "test/fixtures/frameworks/full/cargo-1.95.0-help.txt"
        assertJsonContains "cargo subcommands field" "\"subcommands\"" json
        assertJsonContains "cargo build alias" "\"aliases\":[\"b\"]" json
        assertJsonContains "cargo run alias" "\"aliases\":[\"r\"]" json
        assertJsonContains "cargo test alias" "\"aliases\":[\"t\"]" json
    , testCase "fd full snapshot emits repeated command arguments" $ do
        json <- snapshotJSON "fd" <$> readFile "test/fixtures/frameworks/full/fd-10.4.2-help.txt"
        assertJsonContains "fd exec option" "\"--exec\"" json
        assertJsonContains "fd repeated command arg" "\"argument\":\"<cmd>...\"" json
        assertJsonContains "fd strip cwd optional arg" "\"argument\":\"[=<when>]\"" json
    ]

snapshotJSON :: String -> String -> String
snapshotJSON name content =
  T.unpack . toJSONText $ snapshotCommand name content

snapshotCommand :: String -> String -> Command
snapshotCommand name content =
  Command name [] name (parseUsage content) opts subcommands ""
  where
    opts = parseBlockwise content
    subcommands =
      [ Command subName aliases desc "" [] [] ""
      | Subcommand subName aliases desc <- parseSubcommand content
      ]

assertJsonContains :: String -> String -> String -> IO ()
assertJsonContains label needle haystack =
  assertBool (label ++ ": " ++ needle) (needle `List.isInfixOf` haystack)
