{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CommandArgs (Config (..), ConfigOrVersion (..), configOrVersion)
import Config (setVerbose)
import Control.Exception (try)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import H2OError (H2OError, renderH2OError)
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
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  setEnv "COLUMNS" "1000"
  cfg <- execParser opts >>= initialize
  result <- try (run cfg) :: IO (Either H2OError Text)
  case result of
    Left err -> do
      hPutStrLn stderr (renderH2OError err)
      exitFailure
    Right text -> TIO.putStr text
  where
    opts =
      info
        (configOrVersion <**> helper)
        ( fullDesc
            <> progDesc "Parse help or manpage texts, extract command options, and generate shell completion scripts"
        )

initialize :: ConfigOrVersion -> IO ConfigOrVersion
initialize cfg = do
  case cfg of
    C_ c -> setVerbose (_verbose c)
    _ -> pure ()
  pure cfg
