{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CommandArgs where

import qualified Data.Text as T
import Options.Applicative

-- The boolean corresponds to `skipMan` meaning that
-- it obtains texts from help pages if it's True
-- otherwise it tries both man pages and help texts.
data Input
  = CommandInput String Bool
  | FileInput FilePath Bool
  | JsonInput FilePath

data Config = Config
  { _input :: Input,
    _outputFormat :: OutputFormat,
    _isOutputJSON :: Bool,
    _isPreprocessOnly :: Bool,
    _depth :: Int
  }

data ConfigOrVersion = Version | C_ Config

data OutputFormat = Bash | Zsh | Fish | Json | Native deriving (Eq, Show)

toOutputFormat :: String -> OutputFormat
toOutputFormat s
  | s' == "bash" = Bash
  | s' == "zsh" = Zsh
  | s' == "fish" = Fish
  | s' == "json" = Json
  | otherwise = Native
  where
    s' = T.toLower . T.pack $ s

commandInput :: Parser Input
commandInput =
  CommandInput
    <$> strOption
      ( long "command"
          <> short 'c'
          <> metavar "<string>"
          <> help "Extract CLI options from the help texts or man pages associated with the command. Subcommand pages are also scanned automatically."
      )
    <*> switch
      ( long "skip-man"
          <> help "Skip scanning manpage and focus on help text. Does not apply if input source is a file."
      )

fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "<file>"
          <> help "Extract CLI options form the text file."
      )
    <*> switch
      ( long "skip-man"
          <> help "Skip scanning manpage and focus on help text. Does not apply if input source is a file."
      )

jsonInput :: Parser Input
jsonInput =
  JsonInput
    <$> strOption
      ( long "loadjson"
          <> metavar "<file>"
          <> help "Load JSON file in Command schema."
      )

inputP :: Parser Input
inputP = commandInput <|> fileInput <|> jsonInput

config :: Parser ConfigOrVersion
config =
  C_
    <$> ( Config
            <$> inputP
            <*> ( toOutputFormat
                    <$> strOption
                      ( long "format"
                          <> metavar "{bash|zsh|fish|json|native}"
                          <> showDefault
                          <> value "native"
                          <> help "Select output format of the completion script (bash|zsh|fish|json|native)"
                      )
                )
            <*> switch
              ( long "json"
                  <> help "Output in JSON. Same as --format=json"
              )
            <*> switch
              ( long "debug"
                  <> help "[Debug] Run preprocessing only"
              )
            <*> option
              auto
              ( long "depth"
                  <> metavar "<int>"
                  <> showDefault
                  <> value 4
                  <> help "Set upper bound of the depth of subcommand level"
              )
        )

version :: Parser ConfigOrVersion
version = flag' Version (long "version" <> help "Show version")

configOrVersion :: Parser ConfigOrVersion
configOrVersion = config <|> version
