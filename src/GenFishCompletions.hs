{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Fish shell completion script generation.
--
-- == Known Limitations
--
-- Fish's @__fish_seen_subcommand_from@ function checks if a subcommand name
-- appears anywhere on the command line, regardless of position. This means
-- if the same subcommand name is used at different nesting levels (e.g.,
-- @mycmd sub1@ and @mycmd sub2 sub1@), options may leak between contexts.
-- This is a rare edge case caused by unusual CLI design choices.
module GenFishCompletions (toFishScript, truncateAfterPeriod, makeFishLineOption, makeAncestorCondition, makeNoChildCondition) where

import qualified Data.List as List
import Data.List.Extra (nubOrd)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Type (Command (..), Opt (..), OptName (..), OptNameType (..))

-- -- https://unix.stackexchange.com/questions/296141/how-to-use-a-special-character-as-a-normal-one-in-unix-shells
-- escapeSpecialChars :: Text -> Text
-- escapeSpecialChars s = T.foldl' f s symbols
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
    quotedDesc = (show . truncateAfterPeriod . T.pack) desc
    line = T.strip . T.pack $ printf "complete -c %s %s -d %s %s" cmd optnameAsArgs quotedDesc (optArgToFlag opt)

-- | Generate condition requiring all ancestors to be seen (excluding the root command)
-- e.g., ["mycmd", "sub1"] -> "__fish_seen_subcommand_from sub1"
-- e.g., ["mycmd", "sub1", "subsub1"] -> "__fish_seen_subcommand_from sub1; and __fish_seen_subcommand_from subsub1"
makeAncestorCondition :: [String] -> Text
makeAncestorCondition cmdSeq
  | length cmdSeq <= 1 = ""
  | otherwise = T.intercalate "; and " conditions
  where
    ancestors = drop 1 cmdSeq -- skip root command
    conditions = [T.pack $ printf "__fish_seen_subcommand_from %s" a | a <- ancestors]

-- | Generate condition that no child subcommands have been seen
-- e.g., [subsub1, subsub2] -> "not __fish_seen_subcommand_from subsub1 subsub2"
makeNoChildCondition :: [Command] -> Text
makeNoChildCondition [] = ""
makeNoChildCondition subcmds = T.pack $ printf "not __fish_seen_subcommand_from %s" subcmdsAsTxt
  where
    subcmdsAsTxt = List.intercalate " " [_name c | c <- subcmds]

-- | make a fish-completion line for a root-level option suppressed after a subcommand
makeFishLineRootOption :: String -> [String] -> Opt -> Text
makeFishLineRootOption cmd subcmds opt@(Opt names _ desc) = line
  where
    parts = T.unwords $ map optNameToFishArg names
    quotedDesc = (show . truncateAfterPeriod . T.pack) desc
    subcmdsAsTxt = T.unwords $ map T.pack subcmds
    cond = T.pack $ printf "-n \"not __fish_seen_subcommand_from %s\"" subcmdsAsTxt
    line = T.strip . T.pack $ printf "complete -c %s %s %s -d %s %s" cmd cond parts quotedDesc (optArgToFlag opt)

-- | make a fish-completion line for a subcommand name at any nesting level
-- cmdSeq: full path to parent command (e.g., ["stack", "ls"])
-- For root level: uses __fish_use_subcommand
-- For nested level: requires ancestors seen AND no child subcommands of current level seen
makeFishLineSubcommandNested :: String -> [String] -> [Command] -> Command -> Text
makeFishLineSubcommandNested rootName cmdSeq siblings (Command subname desc _ _ _ _) = line
  where
    template = "complete -k -c %s -n \"%s\" -x -a %s -d %s"
    quotedDesc = show desc
    ancestorCond = makeAncestorCondition cmdSeq
    noChildCond = makeNoChildCondition siblings
    -- Combine conditions: need ancestors seen (if any), and no sibling subcommands seen yet
    condition
      | null cmdSeq || length cmdSeq == 1 = "__fish_use_subcommand"
      | T.null noChildCond = ancestorCond
      | otherwise = ancestorCond <> "; and " <> noChildCond
    line = T.pack $ printf template rootName condition subname quotedDesc

-- | make a fish-completion line for an option under a subcommand at any nesting level
-- cmdSeq: full path to current command (e.g., ["stack", "ls", "snapshots"])
-- childSubcmds: subcommands of the current command (to exclude when those are selected)
makeFishLineSubcommandOptionNested :: String -> [String] -> [Command] -> Opt -> Text
makeFishLineSubcommandOptionNested rootName cmdSeq childSubcmds opt@(Opt names _ desc) = line
  where
    parts = T.unwords $ map optNameToFishArg names
    quotedDesc = (show . truncateAfterPeriod . T.pack) desc
    ancestorCond = makeAncestorCondition cmdSeq
    noChildCond = makeNoChildCondition childSubcmds
    -- Combine conditions: need ancestors seen, and (if has children) no child subcommands seen
    condition
      | T.null ancestorCond = ""  -- should not happen for subcommand options
      | T.null noChildCond = ancestorCond
      | otherwise = ancestorCond <> "; and " <> noChildCond
    condArg = if T.null condition then "" else T.pack $ printf "-n \"%s\"" condition
    line = T.strip . T.pack $ printf "complete -c %s %s %s -d %s %s" rootName condArg parts quotedDesc (optArgToFlag opt)

-- | make a fish-completion line for a subcommand name:: String
makeFishLineSubcommand :: String -> Command -> Text
makeFishLineSubcommand name (Command subname desc _ _ _ _) = line
  where
    template = "complete -k -c %s -n __fish_use_subcommand -x -a %s -d %s"
    quotedDesc = show desc
    line = T.pack $ printf template name subname quotedDesc

-- | make a fish-completion line for an option under a subcommand
makeFishLineSubcommandOption :: String -> String -> Opt -> Text
makeFishLineSubcommandOption cmd subcmd opt@(Opt names _ desc) = line
  where
    parts = T.unwords $ map optNameToFishArg names
    quotedDesc = (show . truncateAfterPeriod . T.pack) desc
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
genFishScriptSubcommands :: String -> [Command] -> Text
genFishScriptSubcommands name subcmds =
  unlineFishCommands [makeFishLineSubcommand name sub | sub <- List.reverse subcmds]

-- | Generate fish completion script for options under a subcommand
genFishScriptSubcommandOptions :: String -> Command -> Text
genFishScriptSubcommandOptions name (Command subname _ _ opts _ _) =
  unlineFishCommands [makeFishLineSubcommandOption name subname opt | opt <- opts]

-- | Recursive helper for multi-level subcommand support
-- rootName: the root command name (e.g., "stack")
-- cmdSeqPrev: ancestor command names up to but not including current (e.g., ["stack", "ls"])
-- siblings: sibling subcommands at the current level (for generating subcommand completions)
toFishScriptHelper :: String -> [String] -> [Command] -> Command -> Text
toFishScriptHelper rootName cmdSeqPrev siblings (Command name _ _ opts subcmds _) =
  T.intercalate "\n" (filter (not . T.null) parts) <> rest
  where
    cmdSeq = cmdSeqPrev ++ [name]
    isRoot = null cmdSeqPrev
    subnames = map _name subcmds

    -- Options for this command
    scriptOptions
      | isRoot && null subcmds = genFishScriptSimple name opts  -- simple case
      | isRoot = genFishScriptRootOptions name subnames opts    -- root with subcommands
      | otherwise = unlineFishCommands [makeFishLineSubcommandOptionNested rootName cmdSeq subcmds opt | opt <- opts]

    -- Subcommand names at this level
    scriptSubcommands
      | null subcmds = ""
      | isRoot = genFishScriptSubcommands name subcmds  -- root level uses __fish_use_subcommand
      | otherwise = unlineFishCommands [makeFishLineSubcommandNested rootName cmdSeq subcmds sub | sub <- List.reverse subcmds]

    parts = [scriptOptions, scriptSubcommands]

    -- Recursively process child subcommands
    rest = T.concat ["\n" <> toFishScriptHelper rootName cmdSeq subcmds sub | sub <- subcmds]

toFishScript :: Command -> Text
toFishScript cmd = escapeDollars . addMeta $ toFishScriptHelper (_name cmd) [] [] cmd
  where
    addMeta txt = "# Auto-generated with h2o\n\n" `T.append` txt

-- | Need to escape $ in fish even if it's quoted.
-- I don't see other characters that require escapes when quoted.
escapeDollars :: Text -> Text
escapeDollars = T.replace "$" "\\$"
