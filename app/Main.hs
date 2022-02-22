{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CommandArgs (configOrVersion)
import qualified Data.Text.IO as TIO
import Io (run)
import Options.Applicative
  ( execParser,
    fullDesc,
    helper,
    info,
    progDesc,
    (<**>),
  )
import System.Environment (setEnv)

main :: IO ()
main = do
  setEnv "COLUMNS" "1000"
  execParser opts >>= run >>= TIO.putStr
  where
    opts =
      info
        (configOrVersion <**> helper)
        ( fullDesc
            <> progDesc "Parse help or manpage texts, extract command options, and generate shell completion scripts"
        )
