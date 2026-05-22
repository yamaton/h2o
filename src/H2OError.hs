-- | Typed errors raised by the h2o library entry points.
--
-- The CLI bundled in @app\/Main.hs@ catches these and prints a
-- 'renderH2OError' message to @stderr@ before exiting with failure,
-- preserving the user-facing UX of the previous @System.Exit.die@
-- calls. Library callers may instead catch them via
-- 'Control.Exception.try' \/ 'Control.Exception.catch' to recover or
-- map them to their own error type.
module H2OError
  ( H2OError (..),
    renderH2OError,
  )
where

import Control.Exception (Exception (displayException))

-- | Failure modes that bubble up from 'Io.run'.
data H2OError
  = -- | Neither @--help@ nor @man@ produced output for the given
    -- command (or subcommand). The 'String' is the human-readable
    -- command/subcommand label used in the error message.
    NoHelpOrMan String
  | -- | A @--loadjson@ file failed schema validation. Carries the
    -- file path and the raw aeson error (already includes a JSON
    -- pointer like @$.options[0]@ for context).
    JsonDecodeFailed FilePath String
  | -- | Help text was fetched but no options, subcommands, or usage
    -- could be recovered. Carries the command name or file label.
    NoExtractableOptions String
  | -- | Recursive subcommand scanning exhausted the configured process
    -- budget. Carries the command path being scanned and the budget limit.
    SubprocessBudgetExhausted String Int
  deriving (Show)

instance Exception H2OError where
  displayException = renderH2OError

-- | Render an 'H2OError' as the user-facing error message. Format
-- matches the previous @die@ output exactly so existing scripts that
-- grep h2o's stderr keep working.
renderH2OError :: H2OError -> String
renderH2OError err = case err of
  NoHelpOrMan name ->
    "Error: No help or man page found for '"
      ++ name
      ++ "'. Is the command installed?"
  JsonDecodeFailed path parseErr ->
    "Error: Cannot decode JSON from '"
      ++ path
      ++ "'. Ensure the file contains a valid Command schema.\n  "
      ++ parseErr
  NoExtractableOptions name ->
    "Error: Could not extract options from '"
      ++ name
      ++ "'. The help text may have an unsupported format."
  SubprocessBudgetExhausted name budget ->
    "Error: Subprocess budget exhausted while scanning '"
      ++ name
      ++ "'. h2o stopped to avoid emitting incomplete output. The current limit is "
      ++ show budget
      ++ " help/man invocations; try a lower --depth value or a higher --subprocess-budget value."
