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

getSubcommandCall :: String -> String -> Text
getSubcommandCall name subname =
  T.unlines
    [ sformat ("          " % string % ") " % string) subname funcName,
      "            return",
      "            ;;"
    ]
  where
    funcName = toBashFuncName [name, subname]

getSubcommandFunc :: String -> Command -> Text
getSubcommandFunc name (Command subname _ _ opts subsubcmds _)
  | null subsubcmds && null opts = ""
  | null subsubcmds = T.concat [prefix, suffix]
  | otherwise = T.concat [prefix, subsubCallPrefix, subsubCallBody, subsubCallSuffix, suffix, subsubFuncs]
  where
    subsubNames = [T.pack subsubname | (Command subsubname _ _ _ _ _) <- subsubcmds]
    optsNames = concat [map (T.pack . _raw) optnames | (Opt optnames _ _) <- opts]
    subsubNamesAndSubOptionsText = T.unwords (subsubNames ++ optsNames)
    funcName = toBashFuncName [name, subname]
    prefix =
      T.unlines
        [ sformat (string % " ()") funcName,
          "{",
          "    local cur word_list"
        ]

    subsubCallPrefix =
      T.unlines
        [ "    local i=1 subcommand_index",
          "",
          "    # take the last word that's NOT starting with -",
          "    while [[ ( $i < $COMP_CWORD ) ]]; do",
          "        local s=\"${COMP_WORDS[i]}\"",
          "        case \"$s\" in",
          sformat ("          " % string % ")") subname,
          "            subcommand_index=$i",
          "            break",
          "            ;;",
          "        esac",
          "        (( i++ ))",
          "    done",
          "",
          "    while [[ ( $subcommand_index < $COMP_CWORD ) ]]; do",
          "        local s=\"${COMP_WORDS[subcommand_index}\"",
          "        case \"$s\" in"
        ]

    subsubCallBody = T.unlines $ map (getSubcommandCall name . _name) subsubcmds

    subsubCallSuffix =
      T.unlines
        [ "          *)  ;; ",
          "        esac",
          "        (( subcommand_index++ ))",
          "    done",
          ""
        ]

    suffix =
      T.unlines
        [ sformat ("    word_list=\" " % stext % "\" ") subsubNamesAndSubOptionsText,
          "    cur=\"${COMP_WORDS[COMP_CWORD]}\"",
          "    COMPREPLY=( $(compgen -W \"${word_list}\" -- \"${cur}\") )",
          "}",
          ""
        ]

    subsubFuncs = T.concat $ map (getSubcommandFunc subname) subsubcmds

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

toBashScript :: Command -> Text
toBashScript (Command name _ _ opts subcmds _)
  | null subcmds = T.concat [mainPrefix, mainSuffix, compStatement]
  | otherwise = T.concat [mainPrefix, mainSubcommandCalls, mainSuffix, subcommandFuncs, compStatement]
  where
    subcommandsAndOptsText = T.unwords [getSubcmdsArray subcmds, getOptsArray opts]
    funcName = toBashFuncName [name]
    mainPrefix =
      T.unlines
        [ meta,
          "",
          sformat (string % "()") funcName,
          "{",
          "    local i=1 cmd cur word_list",
          "    cur=\"${COMP_WORDS[COMP_CWORD]}\"",
          "",
          "    # take the last word that's NOT starting with -",
          "    while [[ ( \"$i\" < \"$COMP_CWORD\" ) ]]; do",
          "        local s=\"${COMP_WORDS[i]}\"",
          "        case \"$s\" in",
          "          -*) ;;",
          "          *)",
          "            cmd=\"$s\"",
          "            ;;",
          "        esac",
          "        (( i++ ))",
          "    done",
          "",
          "    case \"$cmd\" in"
        ]
    mainSubcommandCalls = T.unlines $ map (getSubcommandCall name . _name) subcmds
    mainSuffix =
      T.unlines
        [ "      *)",
          sformat ("          word_list=\" " % stext % "\"") subcommandsAndOptsText,
          "          COMPREPLY=( $(compgen -W \"${word_list}\" -- \"${cur}\") )",
          "          ;;",
          "    esac",
          "",
          "}",
          ""
        ]
    subcommandFuncs = T.concat $ map (getSubcommandFunc name) subcmds

    compStatement =
      T.unlines
        [ "## -o bashdefault and -o default are fallback",
          sformat ("complete -o bashdefault -o default -F " % string % " " % string) funcName name
        ]
    meta = "# Auto-generated with h2o"
