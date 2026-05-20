{-# LANGUAGE OverloadedStrings #-}

module Constants where

import Data.Text (Text)

-- | Error markers strong enough to reject a help/version candidate when they
-- appear at the start of the diagnostic line. Generic words such as
-- "missing" or "unknown" are deliberately excluded because valid help text can
-- use them in ordinary descriptions.
errPrefixes :: [Text]
errPrefixes =
  [ "error:",
    "fatal:",
    "invalid option",
    "illegal option",
    "unrecognized option",
    "unknown option",
    "unknown command"
  ]

-- | Error phrases that are specific enough to match anywhere in the first
-- diagnostic line, including after a command prefix such as
-- @foo: unrecognized option@.
errPhrases :: [Text]
errPhrases =
  [ "command not found",
    "commandnotfound",
    "does not exist",
    "doesn't exist",
    "missing argument",
    "missing required",
    "no such file",
    "not understood",
    "requires an argument",
    "unrecognized command",
    "unrecognized option",
    "unknown command",
    "unknown option"
  ]

-- | Letters used as bullet points
bullets :: [Char]
bullets = ['·', '•', 'o']
