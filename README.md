# H2O: Help to Options

H2O extracts command-line options from help text or man pages, then generates shell completion scripts (bash, zsh, fish) or JSON output. By default, it parses `--help` output directly. Use `--use-man` to try man page lookup before falling back to `--help`.

## Features

- Parses `--help` output or man pages to extract flags, options, arguments, and subcommands
- Generates shell completion scripts for **fish**, **zsh**, and **bash**
- Exports structured CLI information as JSON for tooling integration
- Handles nested subcommands (e.g., `git remote add`)
- Works as the backend for [vscode-H2O](https://marketplace.visualstudio.com/items?itemName=tetradresearch.vscode-h2o)

## Installation

### Prebuilt Binaries

New releases provide versioned binaries from [GitHub Releases](https://github.com/yamaton/h2o/releases).

| Platform | Archive | Executable |
|----------|---------|------------|
| macOS Intel | `h2o-x86_64-apple-darwin.tar.gz` | `h2o-x86_64-apple-darwin` |
| macOS Apple Silicon | `h2o-aarch64-apple-darwin.tar.gz` | `h2o-aarch64-apple-darwin` |
| Linux x86_64 | `h2o-x86_64-unknown-linux-gnu.tar.gz` | `h2o-x86_64-unknown-linux-gnu` |
| Linux arm64 | `h2o-aarch64-unknown-linux-gnu.tar.gz` | `h2o-aarch64-unknown-linux-gnu` |
| Alpine Linux x86_64 | `h2o-x86_64-unknown-linux-musl.tar.gz` | `h2o-x86_64-unknown-linux-musl` |

Each archive has a corresponding `.sha256` file, and the release also includes
an aggregate `SHA256SUMS` file. For example, on Linux:

```bash
sha256sum --check h2o-x86_64-unknown-linux-gnu.tar.gz.sha256
```

The `*-unknown-linux-gnu` archives target GNU/Linux with glibc. The
`*-unknown-linux-musl` archive is statically linked and intended for Alpine Linux
x86_64. The binaries are currently unsigned.

### From Source

Requires [Stack](https://docs.haskellstack.org/):

```bash
git clone https://github.com/yamaton/h2o.git
cd h2o
stack build
stack install  # Installs to ~/.local/bin
```

## Usage

### Basic Examples

```bash
# Generate fish completion from command help
h2o -c grep --format fish > ~/.config/fish/completions/grep.fish

# Generate zsh completion
h2o -c git --format zsh > _git

# Generate bash completion
h2o -c docker --format bash > docker.bash

# Export as JSON
h2o -c curl --format json > curl.json
```

### Parse from File

```bash
# Save help text to file, then parse
man ls | col -bx > ls.txt
h2o -f ls.txt --format fish

# Or parse one of the included samples
h2o -f samples/freebayes.txt --format zsh
```

### Load from JSON

```bash
# Load previously exported JSON
h2o --loadjson curl.json --format fish
```

### All Options

```
Options:
  -c, --command <name>     Parse command help output
  -f, --file <path>        Parse options from a file containing help output
  -s, --subcommand <cmd-sub>  Parse a specific subcommand, e.g., git-log
  --loadjson <path>        Load command data from a JSON file
  --skip-man               Skip man page lookup and parse --help output directly (default)
  --use-man                Try man page lookup before falling back to --help output
  --format <format>        Output format: bash|zsh|fish|json|native (default: native)
  --json                   Output in JSON (same as --format=json)
  --depth <n>              Maximum subcommand nesting depth to scan (default: 4)
  --list-subcommands       List detected subcommands (for debugging)
  --debug                  Show preprocessed text without parsing (for debugging)
  -v, --verbose            Show parser diagnostics
  --version                Show version
  -h, --help               Show help
```

## Output Formats

| Format | Description |
|--------|-------------|
| `fish` | Fish shell completion script |
| `zsh`  | Zsh completion script (`_command` format) |
| `bash` | Bash completion script |
| `json` | Structured JSON (can be reloaded with `--loadjson`) |
| `native` | Human-readable debug format |

## How It Works

H2O uses a two-phase parsing strategy:

1. **Layout Analysis** (primary): Detects column structure in help text using frequency-based heuristics. Handles variations in formatting and alignment.

2. **Parser Combinators** (fallback): Uses structured parsing for lines that don't fit the detected layout.

The pipeline:
```
Input (help/man/file) → Normalize → Parse → Deduplicate → Generate
```

## Pre-generated Completions

Want ready-to-use completions? Check out [h2o-curated-data](https://github.com/yamaton/h2o-curated-data) for pre-generated bash/zsh/fish scripts with manual refinements.

## Development

```bash
stack build           # Build
stack test            # Run tests
stack exec h2o -- -c grep --format fish   # Run
stack haddock         # Generate docs
```

### Running Specific Tests

```bash
stack test --test-arguments "--match 'layoutTests'"
stack test --test-arguments "--match 'optNameTests'"
stack test --test-arguments "--match 'propertyTests'"
```

## Known Limitations

- Fish completions may have conflicts when the same subcommand name appears at different nesting levels
- Assumes at least 3 spaces separate option columns from descriptions
- Some tools with non-standard help formats may not parse correctly

## Related Projects

- [parse-help](https://github.com/sindresorhus/parse-help) - Node.js help text parser
- [fish-shell](https://github.com/fish-shell/fish-shell) - Fish shell with built-in completion system
- [zsh-completions](https://github.com/zsh-users/zsh-completions) - Community zsh completions

## License

MIT
