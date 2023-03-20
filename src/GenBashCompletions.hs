{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenBashCompletions (toBashScript, genBashScript) where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Formatting
import Type (Command (..), Opt (..), OptName (..))

getOptsArray :: [Opt] -> Text
getOptsArray opts = T.unwords $ concatMap (map (T.pack . _raw) . _names) opts

getSubcommandCall :: [String] -> String -> Text
getSubcommandCall cmdSeq subname =
  T.unlines
    [ sformat ("        " % string % ") " % string) subname funcName,
      "            return",
      "            ;;"
    ]
  where
    funcName = toBashFuncName (cmdSeq <> [subname])

getSubcommandFunc :: [String] -> Command -> Text
getSubcommandFunc cmdSeq (Command subname _ _ opts subsubcmds _)
  | null subsubcmds && null opts = ""
  | null subsubcmds = T.concat [prefix, suffix]
  | otherwise = T.concat [prefix, subsubCallPrefix, subsubCallBody, subsubCallSuffix, suffix, subsubFuncs]
  where
    subsubNames = [T.pack subsubname | (Command subsubname _ _ _ _ _) <- subsubcmds]
    optsNames = concat [map (T.pack . _raw) optnames | (Opt optnames _ _) <- opts]
    subsubNamesAndSubOptionsText = T.unwords (subsubNames ++ optsNames)
    funcName = toBashFuncName (cmdSeq ++ [subname])
    prefix =
      T.unlines
        [ sformat (string % " ()") funcName,
          "{"
        ]

    subsubCallPrefix =
      T.unlines
        [ "",
          "    case \"$prev\" in"
        ]

    cmdSeqMore = cmdSeq ++ [subname]
    subsubCallBody = T.unlines $ map (getSubcommandCall cmdSeqMore . _name) subsubcmds
    subsubFuncs = T.concat $ map (getSubcommandFunc cmdSeqMore) subsubcmds

    subsubCallSuffix =
      T.unlines
        [ "    esac",
          ""
        ]

    suffix =
      T.unlines
        [ sformat ("    local word_list=\" " % stext % "\" ") subsubNamesAndSubOptionsText,
          "    COMPREPLY=( $(compgen -W \"${word_list}\" -- \"$cur\") )",
          "}",
          ""
        ]

getSubcmdsArray :: [Command] -> Text
getSubcmdsArray subcmds = T.unwords subnames
  where
    subnames = [T.pack subname | (Command subname _ _ _ _ _) <- subcmds]

genBashScript :: String -> [Opt] -> Text
genBashScript name opts = toBashScript (Command name name "" opts [] "")

toBashFuncName :: [String] -> String
toBashFuncName cmdSeq = '_' : List.intercalate "_" xs
  where
    xs = map (filter Char.isAlphaNum) cmdSeq

bashHeader :: Text
bashHeader = "# Auto-generated with h2o\n\n"

toBashScript :: Command -> Text
toBashScript (Command name _ _ opts subcmds _) =
  T.concat [bashHeader, mainPrefix, mainSubcommandCalls, mainSuffix, subcommandFuncs, compStatement]
  where
    subcommandsText = T.unwords [getSubcmdsArray subcmds]
    subcommandsAndOptsText = T.unwords [getSubcmdsArray subcmds, getOptsArray opts]
    funcName = toBashFuncName [name]
    mainPrefix =
      T.unlines
        [ sformat (string % "()") funcName,
          "{",
          "    local cur prev words cword",
          "    _init_completion -s || return",
          "",
          "    local cmd i subcommands",
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
    mainSubcommandCalls = T.unlines $ map (getSubcommandCall [name] . _name) subcmds

    mainSuffix =
      T.unlines
        [ "    esac",
          "",
          sformat ("    local word_list=\" " % stext % "\"") subcommandsAndOptsText,
          "    COMPREPLY=( $(compgen -W \"${word_list}\" -- \"$cur\") )",
          "}",
          ""
        ]
    subcommandFuncs = T.concat $ map (getSubcommandFunc [name]) subcmds

    compStatement =
      T.unlines
        [ "## -o bashdefault and -o default are fallback",
          sformat ("complete -o bashdefault -o default -F " % string % " " % string) funcName name
        ]
