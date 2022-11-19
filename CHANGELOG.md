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
