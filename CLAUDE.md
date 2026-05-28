# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

H2O (Help to Options) is a Haskell CLI tool that extracts command-line options from help text and man pages, then generates shell completion scripts (bash, zsh, fish) or JSON output. It serves as the backend for the vscode-H2O VS Code extension.

- **Version:** 0.5.0
- **License:** MIT
- **Repository:** https://github.com/yamaton/h2o

## Build Commands

```bash
stack build                    # Build the project
stack test                     # Run all tests
stack exec h2o -- --help       # Run the executable
stack haddock                  # Generate documentation
```

### Running Specific Tests

```bash
stack test --test-arguments '-p "Layout Tests"'
stack test --test-arguments '-p OptName'
stack test --test-arguments '-p "Hedgehog tests"'
stack test --test-arguments '-p "Integrated tests"'
```

### Example Usage

```bash
stack exec h2o -- -c grep --format fish    # Fish completions for grep
stack exec h2o -- -c git --format zsh      # Zsh completions for git
stack exec h2o -- -f samples/grep.txt      # Parse from file
stack exec h2o -- -c stack -s build        # Parse subcommand
```

## Architecture

### Processing Pipeline

```
Input (help/man/file/json)
    ↓
Normalize (unicode, ANSI codes, tabs, bullets)    [Io.hs:normalizeInputText]
    ↓
Split by headers                                  [Layout.hs:splitByHeaders]
    ↓
Per-section layout analysis                       [Layout/ColumnAnalysis.hs]
    ↓
Fallback to parser combinators if layout fails    [HelpParser.hs]
    ↓
Postprocess (deduplicate, fix short opts, add version)  [Postprocess.hs]
    ↓
Generate (bash/zsh/fish/json/native)
```

### Module Map

```
app/Main.hs ── entry point, calls Io.run
    │
    ├── CommandArgs.hs ── CLI argument parsing (optparse-applicative)
    │
    ├── Io.hs ── orchestration: routes input to parsers, dispatches to generators
    │   ├── IoHelper.hs ── runs external commands (help, man) via typed-process
    │   └── Config.hs ── global verbosity flag, help/version option sequences
    │
    ├── Layout.hs ── primary parser: header splitting, blockwise processing
    │   ├── Layout/ColumnAnalysis.hs ── frequency-based column detection
    │   └── Layout/Usage.hs ── usage/synopsis section extraction
    │
    ├── HelpParser.hs ── fallback parser: ReadP combinators for option patterns
    │
    ├── Subcommand.hs ── subcommand name extraction (layout + ReadP)
    │
    ├── Postprocess.hs ── deduplication scoring, short-opt fixing, version fetching
    │
    ├── Type.hs ── Command, Opt, OptName, Subcommand (with JSON instances)
    │
    ├── GenFishCompletions.hs ── fish shell output (handles nested subcommands)
    ├── GenZshCompletions.hs ── zsh shell output (_arguments + _describe)
    ├── GenBashCompletions.hs ── bash shell output (compgen-based)
    └── GenJSON.hs ── JSON output (thin wrapper over aeson)

Shared utilities:
    ├── Utils.hs ── text manipulation, frequency analysis, range operations, tracing, terminal output cleaning
    └── Constants.hs ── error keywords, bullet characters
```

### Core Data Types (Type.hs)

```haskell
data Command = Command
  { _name :: String
  , _description :: String
  , _usage :: String
  , _options :: [Opt]
  , _subcommands :: [Command]   -- recursive: subcommands are Commands
  , _version :: String
  }

data Opt = Opt
  { _names :: [OptName]  -- e.g., ["-v", "--verbose"]
  , _arg :: String       -- e.g., "FILE"
  , _desc :: String
  }

data OptName = OptName
  { _raw :: String       -- e.g., "--help"
  , _type :: OptNameType -- Long | Short | Old | DoubleDash | SingleDash
  }
```

Commands are JSON-serializable (ToJSON/FromJSON instances), enabling caching and IDE integration. The `ToJSON Command` instance conditionally omits empty `subcommands` and `version` fields.

### Key Implementation Details

**Layout/ColumnAnalysis.hs** - The core innovation is frequency-based column detection:
- `getOptionLocations` finds lines starting with dashes
- `getOptionOffsets` detects column alignment via statistical mode
- Two independent methods estimate description offset (from description-only lines and from option lines), with disagreement resolution rules
- 75% alignment threshold: at least 75% of option lines must align with detected offset
- 3-space minimum column separator: `"   "` is the heuristic boundary between option and description
- Handles 2-column (option | description) and 3-column (short | long | description) layouts

**Layout.hs** - Coordinates the parsing pipeline:
- `splitByHeaders` divides text by least-indented lines
- `preprocessAll` tries layout analysis first, falls back to HelpParser for lines that don't fit
- `preprocessSecondAttempt` provides a second fallback layer

**HelpParser.hs** - Handles structured option patterns via ReadP:
- `-h, --help` (multiple names separated by `,` `/` `|` `or`)
- `-o ARG`, `-o=ARG`, `-o<ARG>` (arguments in various formats)
- `--[no-]flag` (boolean toggles)
- Bracketed args: `<ARG>`, `[ARG]`, `{ARG}`, `"ARG"`, `'ARG'`
- `preprocessor` splits option-part from description using heuristic separators (tabs, 2+ spaces, colons)
- `optPart` extracts option names and arguments from the option-part string

**Postprocess.hs** - Deduplication and validation:
- `fixShortOptWithArgWithoutSpace` detects `-Ttagsfile` patterns and splits into `-T` + arg
- `fixDuplicateOpts` scores duplicates by description quality, argument format, and name pattern
- `addVersion` fetches version string by running `<cmd> --version`

**IoHelper.hs** - External command execution:
- Tries man page first, falls back to `--help` (configurable via `--skip-man`)
- Uses `Process.proc` with explicit argument lists (no shell interpolation)
- Terminal output cleaning (ANSI escapes, backspace overstrikes) handled by `Utils.cleanTerminalOutput`
- Multiple help invocation strategies per command: `--help`, `help`, `-help`, `-h`, bare

**GenFishCompletions.hs** - Fish output:
- Recursive `toFishScriptHelper` handles arbitrary subcommand nesting
- Uses `__fish_seen_subcommand_from` / `__fish_use_subcommand` conditions
- `escapeDollars` escapes `$` which fish interprets even in quotes

**GenZshCompletions.hs** - Zsh output:
- Generates `_arguments` calls with `-C` for subcommand support
- Leaf commands get standalone functions; nodes get `_commands` + case dispatch

**GenBashCompletions.hs** - Bash output:
- Uses `_init_completion` and `compgen -W` for completion
- Generates nested functions for subcommand dispatch

### Verbosity and Tracing

The codebase uses a custom tracing system in Utils.hs (not `Debug.Trace` directly):
- `debugMsg`, `infoMsg`, `warnMsg` - trace with level tags, only when `Config.isVerbose` is true
- `debugTrace`, `infoTrace`, `warnTrace` - trace string messages
- `debugShow`, `infoShow`, `warnShow` - trace with `Show` instances
- These are sprinkled throughout parsing code and are invaluable for debugging parse failures

Enable with `--verbose` / `-v` flag.

## Testing

Uses Tasty framework with:
- **Golden tests** (`test/Test/GoldenTests.hs`) - Compare outputs against files in `test/golden/`
- **Property tests** (`test/Test/PropertyTests.hs`) - Hedgehog-based generation for option parsing and range operations
- **Unit tests** - HUnit across multiple files

Test modules:
- `Test.HelpParser.OptNameTests` - 5 cases for option name type classification
- `Test.HelpParser.OptPartTests` - 125+ cases including real-world examples (gzip, tar, rsync, minimap2, etc.) and `expectFail` for known unsupported patterns
- `Test.LayoutTests` - Tab conversion, usage parsing
- `Test.PropertyTests` - Generated long/short/old options, range merging
- `Test.ShellCompletionTests` - Fish/Zsh/Bash generation, ancestor/child conditions
- `Test.GoldenTests` - File-input parsing (rsync, grep, bcftools, snakemake, iqtree), command-input parsing (h2o, mockcmd), JSON-input parsing, shell completion output
- `Test.UtilsTests` - Utility functions (frequency, ranges, splitting, truncation)
- `Test.Helpers` - Shared test constructors (`makeOpt`, `test_optPart`, `test_parseBlockwise`, etc.)

Sample CLI help texts for testing are in `samples/`. Golden reference outputs are in `test/golden/`.

### Test Coverage Notes

Strong coverage for parsing (OptPartTests is extensive). Weak coverage for:
- Postprocessing (deduplication, scoring)
- IO/external command execution
- JSON round-trip serialization
- Error paths and malformed input
- Subcommand extraction logic

See CODE_REVIEW.md for detailed gap analysis.

## Build Configuration

- **Stack** with resolver lts-23.28 (GHC 9.8.4)
- **package.yaml** is the source of truth (generates h2o.cabal via hpack)
- Uses `-Wall` for all GHC warnings
- Exposes a library in addition to the executable

### Key Dependencies

- aeson (JSON serialization)
- optparse-applicative (CLI parsing)
- typed-process (subprocess execution)
- extra (trim, nubSort, nubOrd, stripInfix utilities)
- formatting (Text.Formatting for bash/zsh script generation via `sformat`)
- ordered-containers (ordered map for subcommand deduplication preserving order)
- containers (Data.Map, Data.Set)

## Conventions and Patterns

### Naming in Generator Modules

- `to*` - Top-level entry points (Command -> Text)
- `gen*` - Mid-level functions generating script sections from collections
- `make*` - Low-level functions constructing single lines or pieces

### Error Handling

Currently uses `die` (process exit) for fatal errors and `error` for "impossible" states. See CODE_REVIEW.md for recommendations on improving this for library use.

### Tool-Specific Workarounds

Some tools have hardcoded special handling scattered across files:
- **bazel**: Custom help args in IoHelper.hs, commented-out `--[no-]` parsing in HelpParser.hs
- **micromamba**: Special `Excludes:` handling in HelpParser.hs
- **gatk**: Custom line count threshold in Utils.hs
- **seqtk**: Noted in Config.hs comments (requires different help invocation)

## Known Limitations

1. Fish completions have name collision issues at different subcommand nesting levels (documented in GenFishCompletions.hs)
2. Layout analysis assumes 3-space minimum column separator (fragile for non-standard formatting)
3. `decodeUtf8` in IoHelper.hs crashes on non-UTF-8 output from commands
4. `_isOutputJSON` config field is redundant with `_outputFormat == Json` (exists for deprecated `--json` flag)
5. `parseMany` in Layout.hs is deprecated but still exported without a pragma
