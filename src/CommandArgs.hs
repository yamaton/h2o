{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CommandArgs where

import Data.List.Extra (stripInfix)
import qualified Data.Text as T
import Options.Applicative

-- | Type of external input to `h2o` command
-- Bool here corresponds to `--skipMan` i.e.
-- it obtains texts from help pages if it's True
-- otherwise it searches man pages first then look for help texts.
data Input
  = CommandInput String Bool
  | FileInput FilePath Bool
  | SubcommandInput String String Bool
  | JsonInput FilePath

-- | Config type reflecting `h2o` command options
-- _isOutputJSON is redundant because _outputFormat can be Json
-- But it stays to keep convenient `--json` option.
data Config = Config
  { _input :: Input,
    _outputFormat :: OutputFormat,
    _isOutputJSON :: Bool,
    _isListingSubcommands :: Bool,
    _isPreprocessOnly :: Bool,
    _depth :: Int,
    _verbose :: Bool
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

parseSubcommandPair :: ReadM (String, String)
parseSubcommandPair = eitherReader $ \s ->
  case stripInfix "-" s of
    Just pair -> Right pair
    Nothing -> Left $ "Invalid format: '" ++ s ++ "'. Expected 'command-subcommand' (e.g., git-log)"

-- | Internal type representing input source selection (without skipMan flag)
data InputSource
  = CommandSource String
  | FileSource FilePath
  | SubcommandSource String String
  | JsonSource FilePath

-- | Convert input source and skipMan flag to Input
toInput :: InputSource -> Bool -> Input
toInput (CommandSource cmd) skipMan = CommandInput cmd skipMan
toInput (FileSource path) skipMan = FileInput path skipMan
toInput (SubcommandSource cmd sub) skipMan = SubcommandInput cmd sub skipMan
toInput (JsonSource path) _ = JsonInput path

subcommandSource :: Parser InputSource
subcommandSource =
  uncurry SubcommandSource
    <$> option parseSubcommandPair
      ( long "subcommand"
          <> short 's'
          <> metavar "<command-subcommand>"
          <> help "Extract CLI options from the subcommand-specific help text or man page. Enter a command-subcommand pair, like git-log, as the argument."
      )

commandSource :: Parser InputSource
commandSource =
  CommandSource
    <$> strOption
      ( long "command"
          <> short 'c'
          <> metavar "<string>"
          <> help "Extract CLI options from the help texts or man pages associated with the command. Subcommand pages are also scanned automatically."
      )

fileSource :: Parser InputSource
fileSource =
  FileSource
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "<file>"
          <> help "Extract CLI options from the text file."
      )

jsonSource :: Parser InputSource
jsonSource =
  JsonSource
    <$> strOption
      ( long "loadjson"
          <> metavar "<file>"
          <> help "Load JSON file in Command schema."
      )

inputSourceP :: Parser InputSource
inputSourceP = commandSource <|> fileSource <|> subcommandSource <|> jsonSource

skipManSwitch :: Parser Bool
skipManSwitch =
  switch
    ( long "skip-man"
        <> help "Skip scanning manpage and focus on help text. Does not apply if input source is a file."
    )

inputP :: Parser Input
inputP = toInput <$> inputSourceP <*> skipManSwitch

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
              ( long "list-subcommands"
                  <> help "[Debug] List subcommands"
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
            <*> switch
              ( long "verbose"
                  <> short 'v'
                  <> help "Enable verbose output showing parser diagnostics"
              )
        )

version :: Parser ConfigOrVersion
version = flag' Version (long "version" <> help "Show version")

configOrVersion :: Parser ConfigOrVersion
configOrVersion = config <|> version
