# H2O: Help to Options - Agent Guide

This document provides essential information for AI coding agents working on the H2O project.

## Project Overview

H2O (Help to Options) is a Haskell CLI tool that extracts command-line options from help text and man pages, then generates shell completion scripts (bash, zsh, fish) or JSON output. It serves as the backend for the [vscode-H2O](https://marketplace.visualstudio.com/items?itemName=tetradresearch.vscode-h2o) VS Code extension.

- **Version:** 0.5.0
- **License:** MIT
- **Repository:** https://github.com/yamaton/h2o
- **Language:** Haskell (GHC 9.8.4)
- **Build Tool:** Stack

## Technology Stack

### Core Dependencies
- **GHC 9.8.4** via Stackage LTS-23.28
- **aeson** - JSON serialization for Command/Opt types
- **optparse-applicative** - CLI argument parsing
- **typed-process** - Subprocess execution for running commands
- **text** - Unicode text handling
- **extra** - Utility functions (trim, nubSort)
- **ordered-containers** - OrderedMap for preserving insertion order

### Testing Dependencies
- **tasty** - Test framework
- **tasty-hunit** - Unit tests
- **tasty-golden** - Golden file tests
- **tasty-expected-failure** - Expected failure handling
- **hedgehog** + **tasty-hedgehog** - Property-based testing

## Build Configuration

### Key Files
| File | Purpose |
|------|---------|
| `package.yaml` | **Source of truth** for build configuration (hpack format) |
| `h2o.cabal` | Auto-generated from package.yaml via hpack |
| `stack.yaml` | Stack resolver configuration (LTS-23.28) |

### Build Commands
```bash
# Build the project
stack build

# Run all tests
stack test

# Run executable
stack exec h2o -- --help

# Generate documentation
stack haddock

# Build release binary
stack build --copy-bins
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
Normalize (unicode, ANSI codes, tabs, bullets)
    ↓
Parse (Layout.hs primary, HelpParser.hs fallback)
    ↓
Postprocess (deduplicate, validate)
    ↓
Generate (bash/zsh/fish/json/native)
```

### Module Organization

#### Source Code (`src/`)
| Module | Lines | Purpose |
|--------|-------|---------|
| `Layout.hs` | ~690 | Primary parser using statistical column-based layout analysis |
| `HelpParser.hs` | ~443 | Fallback parser using ReadP combinators |
| `Utils.hs` | ~373 | Text utilities, frequency analysis |
| `Io.hs` | ~231 | Main I/O orchestration |
| `IoHelper.hs` | ~126 | External command execution (help/man) |
| `GenFishCompletions.hs` | ~227 | Fish shell completion generation |
| `GenZshCompletions.hs` | ~212 | Zsh shell completion generation |
| `Postprocess.hs` | ~140 | Deduplication and validation |
| `CommandArgs.hs` | ~143 | CLI argument parsing |
| `Type.hs` | ~129 | Core data types with JSON instances |
| `GenBashCompletions.hs` | ~119 | Bash shell completion generation |
| `Subcommand.hs` | ~114 | Subcommand extraction |
| `Config.hs` | ~33 | Global verbosity flag, help/version options |
| `Constants.hs` | ~27 | Error keywords, bullet characters |
| `GenJSON.hs` | ~11 | JSON output wrapper |
| `Version.hs` | ~11 | Version string (auto-derived from package.yaml) |

#### Executable (`app/`)
- `Main.hs` - Entry point, CLI parsing with optparse-applicative

#### Tests (`test/`)
- `Spec.hs` - Test suite entry point
- `Test.Helpers.hs` - Test utilities
- `Test.HelpParser.OptNameTests.hs` - Option name parser tests
- `Test.HelpParser.OptPartTests.hs` - Option part parser tests
- `Test.LayoutTests.hs` - Layout parsing tests
- `Test.PropertyTests.hs` - Hedgehog property tests
- `Test.ShellCompletionTests.hs` - Shell completion generation tests
- `Test.GoldenTests.hs` - Golden file comparison tests
- `Test.UtilsTests.hs` - Utility function tests

### Core Data Types (`Type.hs`)

```haskell
data Command = Command
  { _name :: String         -- command name
  , _description :: String  -- description of command itself
  , _usage :: String        -- usage string
  , _options :: [Opt]       -- command options
  , _subcommands :: [Command]  -- nested subcommands
  , _version :: String      -- version string
  }

data Opt = Opt
  { _names :: [OptName]     -- e.g., ["-v", "--verbose"]
  , _arg :: String          -- e.g., "FILE"
  , _desc :: String         -- description
  }

data OptName = OptName
  { _raw :: String          -- e.g., "--help"
  , _type :: OptNameType    -- LongType | ShortType | OldType | DoubleDashAlone | SingleDashAlone
  }
```

All types are JSON-serializable (ToJSON/FromJSON instances) enabling caching and IDE integration.

## Code Style Guidelines

### Language Extensions Used
- `OverloadedStrings` - For Text literals
- `DuplicateRecordFields` - For record field name reuse
- `MonadComprehensions` - For cleaner monadic code
- `ScopedTypeVariables` - For explicit type scoping
- `BangPatterns` - For strict evaluation

### Import Style
```haskell
-- Standard library imports first
import qualified Data.Text as T
import Data.Text (Text)

-- Third-party imports
import qualified Data.Aeson as Aeson

-- Local module imports last
import Type (Command(..), Opt(..))
```

### Key Conventions
1. **Record fields** are prefixed with underscore: `_name`, `_options`
2. **Functions** use camelCase: `parseBlockwise`, `toBashScript`
3. **Types** use PascalCase: `OptNameType`, `Command`
4. **Text vs String** - Use `Text` for I/O and processing; `String` for identifiers and simple values
5. **Warnings** - All code compiles with `-Wall` (no warnings)

## Testing Strategy

### Test Categories
1. **Golden Tests** - Compare outputs against reference files in `test/golden/`
   - File-based: Parse sample files, compare output
   - Command-based: Parse real command help, compare output
2. **Property Tests** - Hedgehog-based generation for round-trip testing
3. **Unit Tests** - HUnit for specific parsing behavior

### Test Data
- **206 sample files** in `samples/` - Real help texts and man pages
  - bioinformatics tools (bcftools, samtools, vcftools, etc.)
  - standard Unix tools (grep, docker, stack, etc.)
  - Test fixtures for specific parsing scenarios

### Running Tests
```bash
stack test                    # All tests
stack test --ta '-p X'        # Specific pattern (tasty -p)
```

### CI/CD
- **GitHub Actions** workflow in `.github/workflows/tests.yml`
- Tests run on every push
- GHC 9.8.4 with Stack
- Requires `mockcmd` fixture in `/usr/local/bin/`

## Development Workflow

### Adding New Features
1. Modify source in `src/`
2. Add/update tests in `test/`
3. Run `stack test` to verify
4. Update `CHANGELOG.md` if user-facing

### Version Management
- Version is defined in `package.yaml` (single source of truth)
- `Version.hs` auto-derives from Cabal Paths module
- `CHANGELOG.md` tracks version history

### Release Process
- GitHub Actions workflow `.github/workflows/release.yml`
- Triggered on version tags matching `[0-9]+.[0-9]+.[0-9]+`
- Builds macOS binary and uploads as artifact
- Currently only macOS releases are automated

## Key Implementation Details

### Layout.hs (Primary Parser)
Uses frequency-based column detection:
- `getOptionLocations` - Finds lines starting with dashes
- `getOptionOffsets` - Detects column alignment via mode statistics
- `getMostFrequent` - Enables tolerance for slight misalignment
- Handles 2-column and 3-column layouts automatically
- Assumes 3-space minimum column separator

### HelpParser.hs (Fallback Parser)
Uses ReadP combinators for structured parsing:
- `-h, --help` (multiple names)
- `-o ARG`, `-o=ARG`, `-o<ARG>` (arguments)
- `--[no-]flag` (boolean toggles)
- Bracketed args: `<ARG>`, `[ARG]`, `{ARG}`

### Postprocess.hs
Deduplication scoring:
- Tallies option name frequencies
- Scores by description specificity
- Removes low-scoring duplicates

### Debugging
Global verbosity flag in `Config.hs`:
```haskell
setVerbose :: Bool -> IO ()
isVerbose :: Bool
```

Trace-based debugging functions in `Utils.hs`:
```haskell
debugMsg, infoMsg, warnMsg :: String -> a -> a
```

## Known Limitations

1. **Fish completions** have name collision issues at different subcommand nesting levels
2. **Layout.hs** assumes 3-space minimum column separator
3. **Input validation** - No size limits on input, limited timeout handling
4. **Error handling** - Mix of Maybe/Either/error; some partial functions exist
5. **Magic numbers** - Several thresholds (75% alignment, etc.) lack documentation

## Security Considerations

- Executes external commands to get help text (`--help`, `man`)
- No sandboxing of subprocesses
- Input from files/command output is processed without strict size limits
- No sanitization of generated shell scripts (assumes parsed input is trusted)

## Scripts and Utilities

| Script | Purpose |
|--------|---------|
| `scripts/gen` | Generate fish completion from sample file |
| `scripts/gen.py` | Python helper for completion generation |
| `scripts/htxt`, `scripts/htxt2` | Help text processing utilities |
| `scripts/man2txt` | Man page to text conversion |
| `scripts/man2haddock.sh` | Man page to Haddock conversion |
| `scripts/wrap-h2o` | Wrapper script for h2o |
| `scripts/wrap_aws_completer` | AWS completer wrapper |

## Related Files

- `CLAUDE.md` - Additional guidance for Claude Code
- `CODE_REVIEW.md` - Detailed code review and recommendations
- `CHANGELOG.md` - Version history
- `samples/` - 206+ sample help texts for testing
- `test/golden/` - Expected test outputs
- `test/fixtures/mockcmd` - Mock command for testing
