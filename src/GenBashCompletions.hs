{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Bash shell completion script generation.
--
-- == Naming Conventions
--
-- See "GenFishCompletions" for the @to*@/@gen*@/@make*@ prefix pattern.
module GenBashCompletions (toBashScript, genBashScript) where

import qualified Data.Char  as Char
import qualified Data.List  as List
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Formatting (sformat, stext, string, (%))
import           Type       (Command (..), Opt (..), OptName (..),
                             OptNameType (..))

getOptsArray :: [Opt] -> Text
getOptsArray opts = T.unwords $ concatMap (map (T.pack . _raw) . _names . quote) opts

getSubcommandCall :: [String] -> String -> Text
getSubcommandCall cmdSeq subname =
  T.unlines
    [ sformat ("        " % string % ") " % string) subname funcName,
      "            return",
      "            ;;"
    ]
  where
    funcName = toBashFuncName (cmdSeq <> [subname])

getSubcmdsArray :: [Command] -> Text
getSubcmdsArray subcmds = T.unwords subnames
  where
    subnames = [T.pack subname | (Command subname _ _ _ _ _ _) <- subcmds]

genBashScript :: String -> [Opt] -> Text
genBashScript name opts = toBashScript (Command name [] name "" opts [] "")

toBashFuncName :: [String] -> String
toBashFuncName cmdSeq = '_' : List.intercalate "_" xs
  where
    xs = map (filter Char.isAlphaNum) cmdSeq

quoteShort :: String -> String
quoteShort "-\"" = "-\\\""
quoteShort s     = s

quoteShortType :: OptName -> OptName
quoteShortType (OptName n ShortType) = OptName (quoteShort n) ShortType
quoteShortType optName               = optName

-- | Escape characters bash interprets inside a double-quoted string:
-- backslash, command substitution (@`@), parameter expansion (@$@), and
-- the closing quote (@\"@). Backslash must be escaped first so the
-- backslashes we insert for the other characters are not themselves
-- doubled. Currently the generated bash script does not embed
-- descriptions, but keeping this helper correct prevents future
-- regressions if descriptions are added to the output.
escapeBashDouble :: Text -> Text
escapeBashDouble =
    T.replace "\"" "\\\""
  . T.replace "`"  "\\`"
  . T.replace "$"  "\\$"
  . T.replace "\\" "\\\\"

quote :: Opt -> Opt
quote (Opt names args desc) = Opt (map quoteShortType names) args quotedDesc
  where
    quotedDesc = T.unpack . escapeBashDouble . T.pack $ desc

bashHeader :: Text
bashHeader = "# Auto-generated with h2o\n\n"

toBashScript :: Command -> Text
toBashScript cmd = bashHeader <> toBashScriptHelper [] cmd <> compStatement
  where
    name = _name cmd
    funcName = toBashFuncName [name]
    compStatement =
      T.unlines
        [ "## -o bashdefault and -o default are fallback",
          sformat ("complete -o bashdefault -o default -F " % string % " " % string) funcName name
        ]

toBashScriptHelper :: [String] -> Command -> Text
toBashScriptHelper cmdSeqPrev (Command name _ _ _ opts subcmds _) =
  header
    <> (if null cmdSeqPrev then initializeOnce else "")
    <> (if null subcmds then "" else subcommandCall)
    <> footer
    <> rest
  where
    cmdSeq = cmdSeqPrev ++ [name]
    rest = T.concat (map (toBashScriptHelper cmdSeq) subcmds)
    subcommandsText = T.unwords [getSubcmdsArray subcmds]
    subcommandsAndOptsText = T.unwords [getSubcmdsArray subcmds, getOptsArray opts]
    funcName = toBashFuncName cmdSeq
    header =
      T.unlines
        [ sformat (string % "()") funcName,
          "{"
        ]
    initializeOnce =
      T.unlines
        [ "    local cur prev words cword",
          "    _init_completion -s || return",
          ""
        ]
    subcommandCallPrefix =
      T.unlines
        [ "    local cmd i subcommands",
          sformat ("    local subcommands=\" " % stext % "\"") subcommandsText,
          "",
          "    for (( i=1; i < cword; i++ )); do",
          "        if [[ \" ${subcommands[*]} \" == *\" ${words[i]} \"* ]]; then",
          "            cmd=${words[i]}",
          "            break",
          "        fi",
          "    done",
          "",
          "    case \"$cmd\" in"
        ]
    subcommandCallBody = T.unlines $ map (getSubcommandCall cmdSeq . _name) subcmds
    subcommandCallSuffix =
      T.unlines
        [ "    esac",
          ""
        ]
    subcommandCall = T.concat [subcommandCallPrefix, subcommandCallBody, subcommandCallSuffix]
    footer =
      T.unlines
        [ sformat ("    local word_list=\" " % stext % "\"") subcommandsAndOptsText,
          "    COMPREPLY=( $(compgen -W \"${word_list}\" -- \"$cur\") )",
          "}",
          ""
        ]
