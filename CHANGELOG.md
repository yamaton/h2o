## 0.5.6 (2026-05-21)
- Support Cobra colon-style subcommand sections, clap repeated flag ellipses, and wrapped argparse descriptions that begin with hyphenated text.
- Add framework fixture coverage and JSON smoke tests for clap, Cobra, uv, cargo, fd, and gh help output.
- Split help parsing internals into focused line, option, and metadata modules.

## 0.5.5 (2026-05-21)
- Preserve QIIME/Click status annotations such as `[required]` after type metadata continuations.
- Improve QIIME/Click parsing for long `Choices(...)` metadata blocks before descriptions.

## 0.5.4 (2026-05-21)
- Fix `stack build --copy-bins --pedantic` by removing unused fish helpers and replacing partial list operations in parsing/build paths.

## 0.5.3 (2026-05-21)
- Preserve comma-separated subcommand aliases such as `remove, uninstall` in JSON output.
- Improve QIIME/Click option parsing for wrapped `Choices(...)` metadata before descriptions or default annotations.

## 0.5.2 (2026-05-21)
- Preserve QIIME/Click right-aligned status annotations such as `[required]`, `[optional]`, and `[default: ...]` in option descriptions.

## 0.5.1 (2026-05-21)
- Improve QIIME/Click-style subcommand parsing by joining wrapped descriptions.
- Improve QIIME action option parsing for typed metadata continuations such as `FeatureData[...]`, `Choices(...)`, and `Range(...)`.
- Decode file inputs leniently so non-UTF-8 bytes do not crash parsing.
- Narrow help error-page detection to avoid rejecting normal help text that starts with words like "missing".

## 0.4.9 (2023-10-02)
 - Bump to GHC 9.4 (lts-21.14)
 - Fix errors in zsh/bash outputs by quoting special symbols properly
 - Support suffix ? like '<STR>?' as an option argument

## 0.4.8 (2023-07-23)
 - Fix parser handling empty brackets, like '[]', as an option argument
 - Copyright to "Triton Lab, LLC"

## 0.4.7 (2023-04-24)
 - Bump to GHC 9.2
 - Support nested subcommands in bash outputs
 - Support nested subcommands in zsh outputs

## 0.4.6 (2023-01-08)
- Improve detection of description in an option line
- Fix partial function in utility
- Improve consumption order
- Remove bullets before processing text
- Allow option `-!` and `-_` seen in nano

## 0.4.5 (2022-11-18)
- Improvements in layout-based parsing
- Bugfixes by adding null checks
- Slightly better log messages with emojis ⚠️🛑

## 0.4.4 (2022-11-16)
- Minor parser improvements
    - Exclude suffix ':' in option arguments
    - Include prefix '@' in option arguments
    - Trim whitespaces in version parsing
    - Handle special cases where long options are parenthesized
    - Sanitize by replacing nbsp with ASCII whitespace
    - Fix unwanted consumption of by `argWordBare`
- Allow usage-only command specs

## 0.4.3 (2022-11-02)
- Bugfixes and tunings
    - Better layout parsing by disallowing unrealistic perturbation
    - Assume repeated subcommand sequences like `foo bar bar`
      as parsing glitches.
    - Escape $ in fish string

## 0.4.2 (2022-11-01)
- Revive command options
    - --subcommand
    - --list-subcommands
- Bugfixes around usage parsing
- Update GitHub Actions for auto-testing

## 0.4.1 (2022-10-30)
- Remove unused command options
    - --subcommand
    - --list-subcommands
    - --convert-tabs-to-spaces

## 0.4.0 (2022-10-30)
- Add usage parsing; Command type accomodates usage
- Fix quotes in fish outputs
- Stackage LTS-19.30
- Introduce invalid/errorneus message detection
- bazel support

## 0.3.3 (2022-05-22)
- Bump to GHC 9.0.2 (via Stackage LTS-19.7)
- Tunings to reduce false positives and negatives
- Add 'version' field to the JSON output
- Fix bash output when command name has symbols

## 0.3.2 (2022-02-23)
- Bugfixes and tunings

## 0.3.1 (2022-02-21)
- Bugfixes
- Parameter tuning

## 0.3.0 (2021-02-08)
- Improve layout extraction
- Fix the bug dropping rightly-placed options
- Fix overloading when handling lines with many quotes
- Minor fixes

## 0.2.0 (2021-12-16)
- Support multi-level subcommands
    - [NOTE] bash/zsh/fish outputs are unsupported yet
- Switch to GHC 9.0.1

## 0.1.19 (2021-10-20)
Tune-ups

## 0.1.0.2 (2021-05-21)
Attach statically-linked executable for x86_64 Ubuntu 20.04.
