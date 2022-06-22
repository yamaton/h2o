{-# LANGUAGE OverloadedStrings #-}

module Constants where

import Data.Text (Text)

-- If any of the following word appear in the first line of the output,
-- the command will be discarded as invalid.
errKeywords :: [Text]
errKeywords =
  [
    "error",
    "invalid",
    "unrecognized",
    "not found",
    "unknown",
    "missing",
    "not understood",
    "fatal",
    "no such file",
    "not exist",
    "doesn\'t exist",
    "commandnotfound"
  ]
