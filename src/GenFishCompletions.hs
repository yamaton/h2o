{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenFishCompletions where

import Data.List.Extra (nubOrd)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Type (Command (..), Opt (..), OptName (..), OptNameType (..), Subcommand (..), asSubcommand)
import qualified Data.List as List

-- https://unix.stackexchange.com/questions/296141/how-to-use-a-special-character-as-a-normal-one-in-unix-shells
escapeSpecialSymbols :: Text -> Text
escapeSpecialSymbols s = T.foldl' f s symbols
  where
    f acc c = T.replace (T.singleton c) ("\\" `T.append` T.singleton c) acc
    symbols = "!?#$%&"

argToFlg :: String -> Text
argToFlg "" = ""
argToFlg t
  | "FILE" `T.isInfixOf` upper = " -r"
  | "DIR" `T.isInfixOf` upper = " -r"
  | "PATH" `T.isInfixOf` upper = " -r"
  | "ARCHIVE" `T.isInfixOf` upper = " -r"  -- respect tar
  | otherwise = " -x"
  where
    upper = T.toUpper (T.pack t)

toFishCompPart :: OptName -> Text
toFishCompPart (OptName raw t) = escapedStr
  where
    dashlessName = T.dropWhile (== '-') (T.pack raw)
    chunk = T.unwords $ filter (not . T.null) [toFlag t, dashlessName]
    escapedStr = escapeSpecialSymbols chunk

toFlag :: OptNameType -> Text
toFlag LongType = "-l"
toFlag ShortType = "-s"
toFlag OldType = "-o"
toFlag _ = ""

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
makeFishLineOption cmd (Opt names arg desc) = line
  where
    parts = T.unwords $ map toFishCompPart names
    quotedDesc = T.replace "'" "\\'" (truncateAfterPeriod (T.pack desc))
    line = T.pack $printf "complete -c %s %s -d '%s'%s" cmd parts quotedDesc (argToFlg arg)

-- | make a fish-completion line for a root-level option suppressed after a subcommand
makeFishLineRootOption :: String -> [String] -> Opt -> Text
makeFishLineRootOption cmd subcmds (Opt names arg desc) = line
  where
    parts = T.unwords $ map toFishCompPart names
    quotedDesc = T.replace "'" "\\'" (truncateAfterPeriod (T.pack desc))
    subcmdsAsTxt = T.unwords $ map T.pack subcmds
    cond = T.pack $ printf "-n \"not __fish_seen_subcommand_from %s\"" subcmdsAsTxt
    line = T.pack $ printf "complete -c %s %s %s -d '%s'%s" cmd cond parts quotedDesc (argToFlg arg)

-- | make a fish-completion line for a subcommand name:: String
makeFishLineSubcommand :: String -> Subcommand -> Text
makeFishLineSubcommand cmd (Subcommand subcmd desc) = line
  where
    template = "complete -k -c %s -n __fish_use_subcommand -x -a %s -d '%s'"
    quotedDesc = T.replace "'" "\\'" (T.pack desc)
    line = T.pack $printf template cmd subcmd quotedDesc

-- | make a fish-completion line for an option under a subcommand
makeFishLineSubcommandOption :: String -> String -> Opt -> Text
makeFishLineSubcommandOption cmd subcmd (Opt names arg desc) = line
  where
    parts = T.unwords $ map toFishCompPart names
    quotedDesc = T.replace "'" "\\'" (truncateAfterPeriod (T.pack desc))
    subcmdCondition = T.pack $ printf "-n \"__fish_seen_subcommand_from %s\"" subcmd
    line = T.pack $ printf "complete -c %s %s %s -d '%s'%s" cmd subcmdCondition parts quotedDesc (argToFlg arg)

-- | Generate simple fish completion script WITHOUT subcommands
genFishScriptSimple :: String -> [Opt] -> Text
genFishScriptSimple cmd opts = T.unlines . nubOrd $ [makeFishLineOption cmd opt | opt <- opts]

-- | Generate fish completion script for root-level options that are suppressed after a subcommand
genFishScriptRootOptions :: String -> [String] -> [Opt] -> Text
genFishScriptRootOptions name subnames opts = T.unlines . nubOrd $ [makeFishLineRootOption name subnames opt | opt <- opts]

-- | Generate fish completion script for subcommand names
--
-- [NOTE] The order is reversed because of fish's complete -k specification; the last line comes the first.
--
genFishScriptSubcommands :: String -> [Subcommand] -> Text
genFishScriptSubcommands name subcmds = T.unlines . nubOrd $ [makeFishLineSubcommand name sub | sub <- List.reverse subcmds]

-- | Generate fish completion script for options under a subcommand
genFishScriptSubcommandOptions :: String -> Command -> Text
genFishScriptSubcommandOptions name (Command subname _ opts _) = T.unlines . nubOrd $ [makeFishLineSubcommandOption name subname opt | opt <- opts]

toFishScript :: Command -> Text
toFishScript (Command name _ opts subcmds)
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
