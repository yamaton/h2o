{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenZshCompletions (toZshScript, genZshScript) where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Formatting
import Text.Printf (printf)
import Type
  ( Command (..),
    Opt (..),
    OptName (..),
    OptNameType (..)
  )

zshHeader :: String -> Text
zshHeader cmd = sformat ("#compdef _" % string % " " % string % "\n\n") cmd cmd

zshHeaderOld :: String -> Text
zshHeaderOld = sformat ("#compdef " % string % "\n\n")

quote :: Text -> Text
quote = T.replace "]" "\\]" . T.replace "[" "\\[" . T.replace "'" "'\\''"

getOptAsText :: Opt -> Text
getOptAsText (Opt optnames arg desc)
  | isArgAboutFile = T.concat [formatted, ":file:_files"]
  | otherwise = formatted
  where
    argUppercase = T.toUpper (T.pack arg)
    isArgAboutFile = any (`T.isInfixOf` argUppercase) ["FILE", "PATH", "DIR", "ARCHIVE"]
    raws = map _raw optnames
    optionNames = List.intercalate "," raws
    quotedDesc = quote . T.pack $ desc
    formatted = case raws of
      [raw] -> T.pack $ printf "'%s[%s]'" raw quotedDesc
      _ -> T.pack $ printf "{%s}'[%s]'" optionNames quotedDesc

getSubcommandAsText :: Command -> Text
getSubcommandAsText cmd =
  T.pack $ printf "'%s:%s'" name quotedDesc
  where
    name = _name cmd
    desc = _description cmd
    quotedDesc = quote (T.pack desc)

indent :: Int -> Text -> Text
indent n t = T.replicate n " " `T.append` t

addSuffix :: Text -> Text -> Text
addSuffix suffix line = line `T.append` suffix

genZshBodyOptions :: String -> [Opt] -> Text
genZshBodyOptions _ opts = res
  where
    args = T.unlines (map (indent 4 . getOptAsText) opts)
    containsOldStyle = elem OldType $ concatMap (map _type . _names) opts
    flags = if containsOldStyle then T.empty else "-s "
    template = "args=(\n%s)\n\n_arguments %s$args\n"
    res = T.pack $ printf template args flags

genZshNodeOptionList :: String -> [Opt] -> Bool -> Text
genZshNodeOptionList _ opts isSubcmdsNull =
  T.concat $ map T.unlines [linesPrefix, linesCore, linesSuffix]
  where
    linesPrefix =
      [ "",
        "    _arguments -C \\"
      ]
    linesSuffix
      | isSubcmdsNull =
          [ "        '*: :_files'",
            ""
          ]
      | otherwise =
          [ "        ': :->cmd' \\",
            "        '*:: :->subcmd'",
            ""
          ]
    linesCore = map (addSuffix " \\" . indent 8 . getOptAsText) opts

genZshSubcommandList :: [Command] -> Text
genZshSubcommandList subcommands = res
  where
    textPrefix =
      [ "    function _commands {",
        "        local -a commands",
        "        commands=("
      ]
    textCore = map (indent 12 . getSubcommandAsText) subcommands
    textSuffix =
      [ "        )",
        "        _describe 'command' commands",
        "    }",
        " "
      ]

    res = T.unlines $ concat [textPrefix, textCore, textSuffix]

genZshLeafOptionList :: [String] -> [Opt] -> Text
genZshLeafOptionList cmdSeq opts =
  T.concat $ map T.unlines [header, body, footer]
  where
    header =
      [ sformat ("function _" % string % " {") (List.intercalate "_" cmdSeq),
        "    _arguments \\"
      ]
    body = map (addSuffix " \\" . indent 8 . getOptAsText) opts
    footer =
      [ "        \"*: :_files\"",
        "",
        "}",
        ""
      ]

zshSubcommandOptionCall :: [String] -> String -> Text
zshSubcommandOptionCall cmdSeq subname = T.unlines xs
  where
    accName = List.intercalate "_" cmdSeq
    xs =
      [ sformat ("        (" % string % ")") subname,
        sformat ("            _" % string % "_" % string) accName subname,
        "            ;;",
        ""
      ]

genZshBodySubcommandOptions :: [String] -> [Command] -> Text
genZshBodySubcommandOptions cmdSeq subcommands =
  T.concat [textPrefix, textCore, textSuffix]
  where
    subnames = [subname | (Command subname _ _ _ _ _) <- subcommands]
    textPrefix =
      T.unlines
        [ "    case $state in",
          "    (cmd)",
          "        _commands",
          "        ;;",
          "    (subcmd)",
          "        case $line[1] in"
        ]
    textCore = T.concat $ map (zshSubcommandOptionCall cmdSeq) subnames
    textSuffix =
      T.unlines
        [ "        esac",
          "        ;;",
          "     esac",
          ""
        ]

comments :: Text
comments = "# Auto-generated with h2o\n\n"

genZshScript :: String -> [Opt] -> Text
genZshScript cmd opts = T.concat [header, comments, body]
  where
    header = zshHeaderOld cmd
    body = genZshBodyOptions cmd opts

------------------

toZshScriptHelper :: [String] -> Command -> Text
toZshScriptHelper cmdSeqPrev (Command name _ _ opts [] _) = genZshLeafOptionList accName opts
  where
    accName = cmdSeqPrev ++ [name]
toZshScriptHelper cmdSeqPrev (Command name _ _ opts subcmds _) = txt <> T.concat rest
  where
    txt = T.concat [textFunctionOpening, textSubcommands, textRootOptions, textSubcommandOptionCalls, textFunctionClosing]
    cmdSeq = cmdSeqPrev ++ [name]
    accSeq = List.intercalate "_" cmdSeq
    textFunctionOpening =
      T.unlines
        [ "",
          sformat ("function _" % string % " {") accSeq,
          "    local line state",
          ""
        ]
    textSubcommands = genZshSubcommandList subcmds
    textRootOptions = genZshNodeOptionList name opts (null subcmds)
    textSubcommandOptionCalls = genZshBodySubcommandOptions cmdSeq subcmds
    textFunctionClosing = T.unlines ["}", ""]
    rest = map (toZshScriptHelper cmdSeq) subcmds


toZshScript :: Command -> Text
toZshScript cmd = T.concat [header, comments] <> toZshScriptHelper [] cmd
  where
    header = zshHeader (_name cmd)
