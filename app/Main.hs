{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CommandArgs (configOrVersion)
-- import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import Io (run)
import Options.Applicative
  ( execParser,
    fullDesc,
    helper,
    info,
    progDesc,
    (<**>),
  )

main :: IO ()
main = execParser opts >>= run >>= BL.putStr
  where
    opts =
      info
        (configOrVersion <**> helper)
        ( fullDesc
            <> progDesc "Parse help or manpage texts, extract command options, and generate shell completion scripts"
        )
