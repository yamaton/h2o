{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenFishCompletions (toFishScript, truncateAfterPeriod, makeFishLineOption) where

import qualified Data.List as List
import Data.List.Extra (nubOrd)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Type (Command (..), Opt (..), OptName (..), OptNameType (..), Subcommand (..), asSubcommand)

-- -- https://unix.stackexchange.com/questions/296141/how-to-use-a-special-character-as-a-normal-one-in-unix-shells
-- escapeSpecialSymbols :: Text -> Text
-- escapeSpecialSymbols s = T.foldl' f s symbols
--   where
--     f acc c = T.replace (T.singleton c) ("\\" `T.append` T.singleton c) acc
--     symbols = "!?#$%&~"

optArgToFlag :: Opt -> Text
optArgToFlag (Opt _ arg desc)
  | arg == "" = ""
  | any (`T.isInfixOf` argLowered) ["file", "dir", "path", "archive"] = "-r"
  | any (`T.isInfixOf` descLowered) ["file", "dir", "path"] = "-r"
  | otherwise = "-x"
  where
    argLowered = T.toLower (T.pack arg)
    descLowered = T.toLower (T.pack desc)

-- | Get option name for fish completion.
-- [NOTE] Return empty text if option is only dashes.
-- Fish completion does not seem to support them.
optNameToFishArg :: OptName -> Text
optNameToFishArg (OptName _ SingleDashAlone) = ""
optNameToFishArg (OptName _ DoubleDashAlone) = ""
optNameToFishArg (OptName raw t) = chunk
  where
    dashlessName = T.dropWhile (== '-') (T.pack raw)
    quotedName = T.pack (show dashlessName)
    chunk = T.unwords [optTypeToFlag t, quotedName]

optTypeToFlag :: OptNameType -> Text
optTypeToFlag LongType = "-l"
optTypeToFlag ShortType = "-s"
optTypeToFlag OldType = "-o"
optTypeToFlag _ = ""

truncateAfterPeriod :: Text -> Text
truncateAfterPeriod line
  | ". " `T.isInfixOf` line = T.unwords zs
  | otherwise = line
  where
    (xs, ys) = span (\w -> T.last w /= '.') (T.words line)
    zs = case ys of
      [] -> xs
      s : ss -> if criteria then xs ++ [s, extra] else xs ++ [s]
        where
          len = T.length s
          criteria =
            len >= 3
              && s `T.index` (len - 2) /= '.'
              && s `T.index` (len - 3) == '.' -- like "e.g."
          extra = truncateAfterPeriod (T.unwords ss)

-- | make a fish-completion line for an option
makeFishLineOption :: String -> Opt -> Text
makeFishLineOption cmd opt@(Opt optnames _ desc) = line
  where
    optnameAsArgs = T.unwords $ map optNameToFishArg optnames
    quotedDesc = show (truncateAfterPeriod (T.pack desc))
    line = T.strip . T.pack $ printf "complete -c %s %s -d %s %s" cmd optnameAsArgs quotedDesc (optArgToFlag opt)

-- | make a fish-completion line for a root-level option suppressed after a subcommand
makeFishLineRootOption :: String -> [String] -> Opt -> Text
makeFishLineRootOption cmd subcmds opt@(Opt names _ desc) = line
  where
    parts = T.unwords $ map optNameToFishArg names
    quotedDesc = show (truncateAfterPeriod (T.pack desc))
    subcmdsAsTxt = T.unwords $ map T.pack subcmds
    cond = T.pack $ printf "-n \"not __fish_seen_subcommand_from %s\"" subcmdsAsTxt
    line = T.strip . T.pack $ printf "complete -c %s %s %s -d %s %s" cmd cond parts quotedDesc (optArgToFlag opt)

-- | make a fish-completion line for a subcommand name:: String
makeFishLineSubcommand :: String -> Subcommand -> Text
makeFishLineSubcommand cmd (Subcommand subcmd desc) = line
  where
    template = "complete -k -c %s -n __fish_use_subcommand -x -a %s -d %s"
    quotedDesc = show (T.pack desc)
    line = T.pack $ printf template cmd subcmd quotedDesc

-- | make a fish-completion line for an option under a subcommand
makeFishLineSubcommandOption :: String -> String -> Opt -> Text
makeFishLineSubcommandOption cmd subcmd opt@(Opt names _ desc) = line
  where
    parts = T.unwords $ map optNameToFishArg names
    quotedDesc = show $ truncateAfterPeriod (T.pack desc)
    subcmdCondition = T.pack $ printf "-n \"__fish_seen_subcommand_from %s\"" subcmd
    line = T.strip . T.pack $ printf "complete -c %s %s %s -d %s %s" cmd subcmdCondition parts quotedDesc (optArgToFlag opt)

unlineFishCommands :: [Text] -> Text
unlineFishCommands = T.unlines . nubOrd . filter (not . T.null)

-- | Generate simple fish completion script WITHOUT subcommands
genFishScriptSimple :: String -> [Opt] -> Text
genFishScriptSimple cmd opts =
  unlineFishCommands [makeFishLineOption cmd opt | opt <- opts]

-- | Generate fish completion script for root-level options that are suppressed after a subcommand
genFishScriptRootOptions :: String -> [String] -> [Opt] -> Text
genFishScriptRootOptions name subnames opts =
  unlineFishCommands [makeFishLineRootOption name subnames opt | opt <- opts]

-- | Generate fish completion script for subcommand names
--
-- [NOTE] The order is reversed because of fish's complete -k specification; last call is displayed first.
genFishScriptSubcommands :: String -> [Subcommand] -> Text
genFishScriptSubcommands name subcmds =
  unlineFishCommands [makeFishLineSubcommand name sub | sub <- List.reverse subcmds]

-- | Generate fish completion script for options under a subcommand
genFishScriptSubcommandOptions :: String -> Command -> Text
genFishScriptSubcommandOptions name (Command subname _ _ opts _ _) =
  unlineFishCommands [makeFishLineSubcommandOption name subname opt | opt <- opts]

toFishScript :: Command -> Text
toFishScript (Command name _ _ opts subcmds _)
  | null subcmds = addMeta $ genFishScriptSimple name opts
  | otherwise = addMeta $ T.intercalate "\n\n\n" (filter (not . T.null) scriptsAll)
  where
    subnames = map _name subcmds
    subcommands = map asSubcommand subcmds
    scriptRootOptions = genFishScriptRootOptions name subnames opts
    scriptSubcommands = genFishScriptSubcommands name subcommands
    scriptSubcommandOptions = [genFishScriptSubcommandOptions name subcmd | subcmd <- subcmds]
    scriptsAll = [scriptRootOptions, scriptSubcommands] ++ scriptSubcommandOptions
    addMeta txt = "# Auto-generated with h2o\n\n" `T.append` txt
