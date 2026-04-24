# H2O Code Review

_Last updated: 2026-04-24 against commit `84c60e6`._

## Executive Summary

H2O is a Haskell CLI that extracts CLI options from help/man text and emits
shell completions (bash/zsh/fish) or JSON. Since the previous review, the
most severe findings (`Process.shell` injection, `die` in library code,
reachable `error` calls, `List.nub` quadratic blowup) have all been closed
out, and a number of robustness features have been added (subprocess
budget / 10 s timeout / stdin-close / JSON schema validation via `FromJSON`).

What remains is mostly clean-up: lingering partiality around UTF-8 decoding,
a handful of user-visible cosmetic bugs, scattered tool-specific workarounds,
and outdated CI configuration. This document tracks those, together with the
status of every item from the previous review.

---

## Status of Previously Flagged Issues

### ✅ Resolved

| Prev # | File(s) | Fix |
|--------|---------|-----|
| 1 | `IoHelper.hs` | `Process.shell` fully replaced with `Process.proc`; terminal-cleanup moved into pure `Utils.cleanTerminalOutput`. |
| 4 | `Io.hs`, `IoHelper.hs` | `die` replaced by the `H2OError` exception type (`H2OError.hs`); `Main.hs` catches and renders at the CLI boundary. Library callers can now `try`/`catch`. |
| 5 | `Type.hs`, `Utils.hs`, `Postprocess.hs` | `toOptionNameType :: Maybe`, `topTenPercentile :: Maybe`, `nameScore` returns `0` for empty input instead of `error`. Tests cover the new totality. |
| 11 | `Layout.hs`, `HelpParser.hs` | All `List.nub` call sites migrated to `nubOrd` / `nubSort` from `extra`. |

Additional hardening delivered since the previous review:

- **Subprocess fan-out budget** (`Io.hs:145-153`, `subprocessBudget = 500`,
  `maxSubcandidatesPerLevel = 100`) stops runaway recursion on noisy
  subcommand trees.
- **Per-invocation 10 s timeout** + **stdin closed before exec**
  (`IoHelper.hs:32-57`) prevents h2o from hanging on commands that wait on
  input or never return.
- **JSON schema validation** (`Type.hs:78-95`) rejects empty `names` arrays
  and non-dash-prefixed names with informative aeson errors instead of
  crashing at use-time.
- **Zsh description quoting** correctly escapes `\`, `[`, `]`, `'` in the
  right order (`GenZshCompletions.hs:38-44`), with regression coverage in
  `Test.ShellCompletionTests`.
- **Process startup exceptions** are caught in `runProcessSafe`
  (`IoHelper.hs:45-57`), so a missing `man` binary no longer surfaces as an
  unhandled `SomeException`.

### ⚠ Partially resolved

| Prev # | Status |
|--------|--------|
| 2 (`unsafePerformIO` verbose flag) | Documented in-source with `{-# NOINLINE #-}` and rationale; new `getVerbose :: IO Bool` accessor for IO callers. The `isVerbose :: Bool` path is still `unsafePerformIO`-based and remains a compromise. |
| 3 (UTF-8 decoding) | See new issue **B3** – `decodeUtf8` is still used directly and is unsafe in `getMan`. |
| 10 (`fixDuplicateOpts` efficiency) | Still marked `[FIXME] inefficient`. Logic was tightened (dedup now gated on full name-list equality, not single-name overlap), but the O(m·n) cost is unchanged. |

### ❌ Still open

Carried over verbatim from the previous review – see the detailed list below
under **Carry-Over Items**.

---

## New Findings

### B1 — Root `Command.description` is set to the command name

**File**: `Io.hs:165`
**Severity**: Medium

```haskell
(cmd, status) <- getCommandRec budget depth useMan [name] name "placeholder" content
--                                              ^^^^^  ^^^^  ^^^^^^^^^^^^^
--                                              cmdSeq desc  upperContent
```

The fifth argument (`desc`) is the command name itself; `"placeholder"` is
the `upperContent` sentinel used to make the "page differs from parent"
check vacuously pass at the root. The effect is that every root `Command`
ends up with `description == name`:

```
$ head -5 test/golden/rsync.txt
Name:  rsync-input
Desc:  rsync-input      <-- should be the tool's one-line summary
```

**Recommendation**: extract the description from the help text (first
non-empty line of the first non-usage block is usually it) or, if
extraction is out of scope for now, pass `""` and teach `toNativeTextRec`
to omit the `Desc:` line when empty. Golden outputs will need regeneration.

### B2 — `addVersion` spawns pointless subprocesses on file/JSON input

**Files**: `Postprocess.hs:151-159`, `Io.hs:57-80`
**Severity**: Low

`fixCommand = addVersion . fixOpts` runs unconditionally after parsing, and
`addVersion` always calls `getVersion name`. For `FileInput
"samples/grep-input.txt"` the derived `name` is `grep-input` (via
`takeBaseName`), so h2o spawns `grep-input --version`, `grep-input version`,
`grep-input -version` – all of which fail. Every failure round-trips through
`runProcessSafe`, consuming part of the 10 s budget per invocation and
occupying a slot of the 500-call `subprocessBudget`.

**Recommendation**: gate `addVersion` on input source. Either pass an
`Input` (or a `shouldFetchVersion :: Bool`) down to `fixCommand`, or split
`fixCommand` into `fixCommandPure` and `enrichWithVersion` and only compose
the latter in the `CommandInput`/`SubcommandInput` paths of `Io.run`.

### B3 — UTF-8 decoding is still unsafe; `getMan` has no guard

**File**: `IoHelper.hs:104-105, 142`
**Severity**: Medium

All three call sites still use `TLE.decodeUtf8` directly:

```haskell
-- fetchHelpInfo
let stdoutText = Utils.cleanTerminalOutput . TL.toStrict . TLE.decodeUtf8 $ stdout
let stderrText = Utils.cleanTerminalOutput . TL.toStrict . TLE.decodeUtf8 $ stderr

-- getMan
| otherwise -> return . Utils.cleanTerminalOutput . TL.toStrict . TLE.decodeUtf8 $ stdout
```

`fetchHelpInfo` is shielded by the `try` in `getHelpTemplateMeta`
(`IoHelper.hs:87`), but `Text` is evaluated lazily and the `UnicodeException`
can in principle escape if a consumer forces the field outside that `try`.
`getMan`, called from `getManAndHelp` / `getManAndHelpSub`, has **no** `try`
wrapper: a man page with non-UTF-8 bytes (Latin-1 locales, embedded binary
noise, older `groff` output on macOS, etc.) crashes h2o outright.

**Recommendation**:

```haskell
import Data.Text.Encoding.Error (lenientDecode)
-- ...
TL.toStrict . TLE.decodeUtf8With lenientDecode $ stdout
```

Apply to all three sites. The `U+FFFD` replacement character is fine for a
parser that already tolerates arbitrary whitespace / punctuation.

### B4 — `words name` silently splits multi-token command names

**File**: `IoHelper.hs:115-117`
**Severity**: Low

```haskell
cmdSeq   = name : args
cmdWords = words name ++ filter (not . all (== ' ')) args
pc       = Process.proc (head cmdWords) (tail cmdWords)
```

Two undocumented behaviours:

1. `name = "sudo foo"` is split on whitespace, so library users can pass a
   wrapper prefix and it works. Neither the README nor the Haddock mentions
   this; it is de-facto behaviour.
2. If a library caller passes `name = ""` together with only whitespace
   args (the `[" "]` sentinel in `Config.helpOptions`), `cmdWords` is empty
   and `head cmdWords` diverges.

**Recommendation**: document the prefix-splitting behaviour in `getHelp`'s
Haddock, and harden the fallthrough – e.g.

```haskell
case cmdWords of
  exe : rest -> Process.proc exe rest
  []         -> throwIO (NoHelpOrMan name)
```

### B5 — `parseSubcommandPair` only splits on the first hyphen

**File**: `CommandArgs.hs:48-52`
**Severity**: Low

```haskell
parseSubcommandPair = eitherReader $ \s ->
  case stripInfix "-" s of
    Just pair -> Right pair
    Nothing   -> Left $ "Invalid format: '" ++ s ++ "'. ..."
```

`docker-compose-up` parses as `("docker", "compose-up")`. The existing help
string only promises `git-log`-style pairs, so this is technically by spec,
but tools with hyphens in their root name (`docker-compose`,
`kubectl-krew`, …) cannot be addressed at all.

**Recommendation**: either document the single-hyphen limitation explicitly
in the `--subcommand` help text, or accept a second separator (e.g.
`docker-compose--up` or `docker-compose/up`) for multi-token commands.

### D1 — `hasErrorMessageAtTop` uses overly generic English keywords

**Files**: `Utils.hs:236-244`, `Constants.hs:9-23`
**Severity**: Low

`Constants.errKeywords` contains `"missing"`, `"not found"`, `"unknown"`,
`"fatal"`, etc., and a command's help text is rejected if any of these
appears in the first 100 characters of the first line. The author's own
`[TODO] Scrutinize this as it's now used for critical purposes` marker
(`Utils.hs:235`) acknowledges the hazard.

Real-world false positives exist – e.g. a tool whose first help line is
_"Show missing dependencies …"_ would be rejected as an error page.

**Recommendation**: tighten the match to anchored patterns, e.g.

```haskell
errPatterns = [ "error:", "fatal:", "unknown option", "command not found"
              , "no such file", "unrecognized option" ]
```

and require `T.isPrefixOf` on the trimmed first line rather than
`T.isInfixOf` on a 100-char window.

### D2 — CI workflow inconsistencies

**Files**: `.github/workflows/tests.yml`, `.github/workflows/release.yml`
**Severity**: Low

| Item | `tests.yml` | `release.yml` |
|------|-------------|---------------|
| `actions/checkout` | `v4` | `v3` |
| Haskell setup action | `haskell-actions/setup@v2` | `haskell/actions/setup@v2` (deprecated org) |
| `actions/upload-artifact` | n/a | `v3` (deprecated, v4 required from 2024-11) |
| `actions/cache` | n/a | `v3` |

Release builds will start failing once GitHub retires the deprecated actions.

**Recommendation**: align both workflows on `actions/checkout@v4`,
`haskell-actions/setup@v2`, and `actions/upload-artifact@v4`.

### D3 — Release workflow only targets macOS

**File**: `.github/workflows/release.yml`
**Severity**: Low

The matrix builds a single `x86_64-apple-darwin` binary. The `package.yaml`
comments already sketch a musl-based static Linux build
(`utdemir/ghc-musl:v25-ghc925`), but it is not wired into CI. ARM macOS
(`aarch64-apple-darwin`) and Windows are also absent.

**Recommendation**: at minimum add Linux `x86_64` (musl static) to the
matrix; ideally also `aarch64-apple-darwin`. The `vscode-H2O` extension
audience is primarily Linux and macOS users.

### D4 — Library dependency bounds are fully open

**File**: `package.yaml:22-33`
**Severity**: Low

`aeson`, `text`, `typed-process`, `optparse-applicative`, `formatting`,
`ordered-containers`, `extra` are all listed without upper bounds. For an
executable shipped as a static binary this is fine, but h2o also exposes a
`library` stanza with 18 modules, which means downstream consumers inherit
the unbounded constraints.

**Recommendation**: add at least major-version upper bounds (e.g. `aeson <
2.3`, `text < 3`, `typed-process < 0.3`) to protect downstream builds.

### D5 — `subprocessBudget` / `maxSubcandidatesPerLevel` are hardcoded

**File**: `Io.hs:140-153`
**Severity**: Low

The 500-call budget and 100-per-level cap are sensible defaults, but there
is no CLI knob. For tools with very deep / wide subcommand trees (e.g.
`az`, `gcloud alpha`) the budget silently truncates results with only a
`warnTrace` emitted in verbose mode.

**Recommendation**: expose `--max-subprocesses` and `--max-subcommands-per-level`
CLI options, or at least emit a non-verbose `stderr` warning when the
budget is exhausted so users know output may be incomplete.

---

## Carry-Over Items

These were flagged in the previous review and are still open.

### C6 — Incomplete Unicode space normalisation

**File**: `Utils.hs:48` · **Severity**: Low

```haskell
unicodeSpacesToAscii = T.replace "\x00a0" " "
```

Still only handles U+00A0 (NBSP). U+2002 (en space), U+2003 (em space),
U+2007 (figure space), U+200B (ZWSP), U+202F (narrow NBSP) all occur in
real-world man pages.

### C7 — Tool-specific parsing scattered across four files

**Files**: `IoHelper.hs:129` (bazel), `HelpParser.hs:60-64` (micromamba),
`Utils.hs:250` (gatk), plus `Config.hs:70,75` comments alluding to seqtk.
**Severity**: Low (Maintainability)

```haskell
-- IoHelper.hs
| name == "bazel" = getHelpTemplate name [["help", subname, "--long"]]
-- HelpParser.hs
| (x : xs) == "Excludes:" = do ...           -- micromamba
-- Utils.hs
| name == "gatk" = length xs >= 4
```

Adding support for a new tool means touching at least three different
source files.

**Recommendation**: collect all tool overrides into a single record, e.g.
`Map String ToolOverride`, loaded from `Config.hs` (compile-time) or a
YAML/JSON file next to the binary (runtime).

### C8 — Redundant `_isOutputJSON` field

**File**: `CommandArgs.hs:23-27`, `Io.hs:43-46` · **Severity**: Low

`_isOutputJSON` exists only for the deprecated `--json` flag and is always
translated into `_outputFormat = Json` on the first iteration of `run`.

**Recommendation**: drop the field, parse `--json` as a synonym for
`--format=json` directly, remove the recursive `run` call.

### C9 — `parseMany` is deprecated but has no pragma

**File**: `Layout.hs:43, 190-200` · **Severity**: Low

The Haddock says `[Deprecated]` but there is no `{-# DEPRECATED #-}`
pragma, so library consumers get no compile-time warning.

```haskell
{-# DEPRECATED parseMany "Use parseBlockwise instead" #-}
parseMany :: String -> [Opt]
```

### C12 — Discarded duplicates are only reported in verbose mode

**File**: `Postprocess.hs:72-89` · **Severity**: Low

Users who notice a missing option get no signal unless they re-run with
`--verbose`. A one-line `stderr` summary (e.g.
`"Dropped 3 duplicate options. Use --verbose for details."`) would reduce
confusion.

### C13 — Subcommand word acceptance vs. documented intent

**File**: `Subcommand.hs:60-63` · **Severity**: Low

```haskell
-- [NOTE] Assume subcommand starts with lowercase
-- Replace with the commented line if you want (uppercase OR lowercase) instead
x <- satisfy $ \c -> toLower c `elem` lowercase
```

The code accepts both cases (because it lowercases before comparing), but
the comment says lowercase only. Either fix the comment or tighten the
predicate to `c `elem` lowercase`.

### C14 — Unexplained magic numbers

| File | Number | Context |
|------|--------|---------|
| `HelpParser.hs:97` | `8` | Max quote-char count before `argWordQuoteHelper` bails, to avoid backtracking blow-up. |
| `HelpParser.hs:271` | `5` | Max number of arg tokens per option. |
| `Layout.hs:86`, `Layout/Usage.hs:95` | `80` | "Infinite" indent used for blank lines in indentation analysis. |
| `Subcommand.hs:38` | `50` | Same concept, different constant. |
| `Layout/ColumnAnalysis.hs:217` | `75` | Alignment-percentage threshold (well documented in the module header). |

**Recommendation**: name them (`maxQuoteChars`, `maxOptionArgs`,
`blankLineIndent`, `subcommandMaxOffset`) and cross-reference the
Haddock. The `75%` threshold is already a positive example.

### C15 — Four-equation `ToJSON Command` / `toEncoding`

**File**: `Type.hs:115-132` · **Severity**: Low

Eight equations total to handle the empty/non-empty × `subcommands`/`version`
product. A single equation with list-comprehension guards is easier to
maintain:

```haskell
toJSON (Command name desc usage opts subs ver) =
  object $ [ "name" .= name, "description" .= desc
           , "usage" .= usage, "options" .= opts
           ]
        ++ [ "subcommands" .= subs | not (null subs) ]
        ++ [ "version"     .= ver  | ver /= "" ]
```

---

## Previously Flagged Issues (Verified Safe)

Unchanged from the prior review; included for completeness.

| Category | Files | Why Safe |
|----------|-------|----------|
| `fromJust` usage | `Layout/ColumnAnalysis.hs`, `Layout/Usage.hs` | Guards ensure `isJust` before evaluation. |
| `!!` indexing | `Layout/ColumnAnalysis.hs`, `Layout.hs` | Indices derived from the same list; covered by bounds. |
| `maximum` / `minimum` | `Utils.hs`, `Layout.hs`, `Layout/Usage.hs`, `Postprocess.hs` | All have empty-list guards. |
| `smartUnwords` logic | `Utils.hs` | `length s > 1` guard ensures `init s` is non-empty. |

---

## Test Coverage

The suite has grown appreciably. New coverage since the previous review:

- `toOptionNameType` across all branches including the new `Nothing` case.
- `FromJSON Opt` rejects empty `names`, non-dash names, with error messages
  that mention the offending value.
- `Postprocess.fixDuplicateOpts` for three scenarios (strict-loser
  discarded, tied winners kept, `-h` shared across two different opts kept).
- `removeBackspaceOverstrikes` and `stripAnsiEscapes` covering overstrikes,
  CSI colour codes, erase-line, bare ESC, and empty input.
- `H2OError` rendering and `try`/`catch` round-trip.
- Zsh description quoting edge cases (backslash, brackets, backslash-before-bracket).
- Empty-input handling in `Layout.preprocessBlockwise` / `parseBlockwise`.

### Remaining gaps

| Module | Gap |
|--------|-----|
| `Postprocess.hs` | `fixShortOptWithArgWithoutSpace` has a single test; `addVersion` has none (including the "should not run for file input" behaviour proposed in B2). |
| `Io.hs` | No unit test for each `Input` variant of `run`; coverage is golden-only. Adding a round-trip that feeds `run` a `Config` and asserts the `H2OError` variant for malformed input would be cheap. |
| `IoHelper.hs` | No test for the 10 s timeout path or for `runProcessSafe` error propagation. Can be exercised with a mock binary that `sleep 30`s. |
| `Subcommand.hs` | `parseSubcommand` / `getAlignedLines` have no direct tests (only `firstTwoWordsLoc`). |
| `Type.hs` | No JSON round-trip property test for `Command` with all four `subcommands`/`version` combinations – exactly the permutations exercised by the four redundant `ToJSON` equations. |
| `CommandArgs.hs` | `parseSubcommandPair` has no unit test; B5's single-hyphen behaviour is uncovered. |
| `Layout/ColumnAnalysis.hs` | Offset detection and the 75 % threshold are only exercised transitively through golden tests. |

---

## Feature Proposals

Carried forward from the previous review – still applicable.

### A. `--stdin` input mode

```bash
some-command --help | h2o --stdin --format fish
```

Makes h2o composable with other tooling and simplifies the common case
where the user already has the help text.

### B. Shell completion for `h2o` itself

`h2o --completions {bash|zsh|fish}` would dogfood the tool and give users
an immediate completion install step.

### C. Configurable tool overrides

Replaces the bazel / micromamba / gatk / seqtk special cases (C7) with a
single data structure – optionally user-extensible at runtime.

### D. On-disk cache of parsed results

`(command, version, source_hash) -> Command` cache. JSON serialisation
already exists; the main decision is cache location (`$XDG_CACHE_HOME/h2o/`
is a reasonable default).

### E. `--validate` mode

Runs `fish -n` / `bash -n` / `zsh -n` on the generated completion script to
catch syntax errors before installation.

---

## Prioritised Action List

| Priority | Item | Area |
|----------|------|------|
| Medium | **B3** — `decodeUtf8With lenientDecode` everywhere, especially `getMan` | Correctness |
| Medium | **B1** — Stop using the command name as the root `description` | Correctness |
| Medium | **B2** — Skip `addVersion` for `FileInput` / `JsonInput` | Performance / UX |
| Low | **C7** — Collapse tool overrides into one structure | Maintainability |
| Low | **C8** — Remove `_isOutputJSON` field | Design |
| Low | **C15** — Collapse `ToJSON Command` equations | Maintainability |
| Low | **C9** — Add `{-# DEPRECATED #-}` pragma to `parseMany` | API hygiene |
| Low | **D2** — Align CI action versions | Ops |
| Low | **D3** — Add Linux (musl) target to release workflow | Distribution |
| Low | **B4** — Document and harden `words name` split | Robustness |
| Low | **B5** — Document single-hyphen split in `--subcommand` help | UX |
| Low | **C6** — Cover more Unicode space characters | Parsing quality |
| Low | **C14** — Name the magic numbers | Readability |
| Low | **D4** — Add major-version upper bounds | Packaging |
| Low | **D1** — Anchor `errKeywords` matching | Parsing quality |
| Low | **C12** — Non-verbose stderr summary for discarded duplicates | UX |
| Low | **D5** — Expose `subprocessBudget` / per-level caps as CLI flags | UX |
| Low | **C13** — Resolve lower/uppercase comment vs. behaviour in `Subcommand.hs` | Consistency |
| Low | Fix the `[FIXME] inefficient` in `Postprocess.fixDuplicateOpts` | Performance |
