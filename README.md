# H2O: Help to Options

H2O extracts command-line options from help text and man pages, then generates shell completion scripts (bash, zsh, fish) or JSON output.

## Features

- Parses `--help` output and man pages to extract flags, options, arguments, and subcommands
- Generates shell completion scripts for **fish**, **zsh**, and **bash**
- Exports structured CLI information as JSON for tooling integration
- Handles nested subcommands (e.g., `git remote add`)
- Works as the backend for [vscode-H2O](https://marketplace.visualstudio.com/items?itemName=tetradresearch.vscode-h2o)

## Installation

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
# Generate fish completion from command's help/man
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

# Or pipe directly (using samples)
h2o -f samples/grep.txt --format zsh
```

### Load from JSON

```bash
# Load previously exported JSON
h2o --loadjson curl.json --format fish
```

### All Options

```
h2o - Extract CLI options from help text

Usage: h2o ((-c|--command COMMAND) | (-f|--file FILE) | --loadjson FILE)
           [(-s|--subcommand SUBCOMMAND)] [--format FORMAT] [--depth N]
           [--skip-man] [--list-subcommands] [--preprocess] [-v|--verbose]

Options:
  -c, --command COMMAND    Command name to parse
  -f, --file FILE          Parse help text from file
  --loadjson FILE          Load Command from JSON file
  -s, --subcommand CMD     Parse a specific subcommand
  --format FORMAT          Output format: fish|zsh|bash|json|native (default: fish)
  --depth N                Subcommand recursion depth (default: 0)
  --skip-man               Skip man pages, use --help only
  --list-subcommands       List detected subcommands
  --preprocess             Show parsed option-description pairs (debug)
  -v, --verbose            Enable verbose output
  --version                Show version
  --help                   Show help
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
