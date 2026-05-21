# CLI framework fixtures

These fixtures are representative help-text excerpts used to lock parser
behavior for common CLI framework styles without depending on locally installed
command versions in CI.

- `cobra-gh-help.txt`: GitHub CLI 2.63.1-style Cobra root help.
- `cobra-gh-auth-help.txt`: GitHub CLI 2.63.1-style Cobra nested command help.
- `argparse-json-tool-help.txt`: Python `json.tool` argparse help style.
- `clap-uv-help.txt`: uv-style clap help with command and option sections.
- `clap-cargo-help.txt`: cargo-style clap help with command aliases.
- `clap-fd-help.txt`: fd 10.4.2-style clap help with repeated flags and repeated command arguments.

The `expected/` files list option names and subcommand names that must be
present in parsed output for each snapshot. They intentionally check extraction
coverage without making descriptions golden-sensitive.

The `full/` files are versioned snapshots of locally available CLI help output
used to catch broad extraction gaps for representative Cobra and clap tools.
