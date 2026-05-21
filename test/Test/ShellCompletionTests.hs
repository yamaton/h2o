{-# LANGUAGE OverloadedStrings #-}

module Test.ShellCompletionTests (tests) where

import qualified Data.Text as T
import qualified GenBashCompletions as GenBash
import qualified GenFishCompletions as GenFish
import qualified GenZshCompletions as GenZsh
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Type (Command (..), Opt (..), OptName (..), OptNameType (..))

tests :: TestTree
tests =
  testGroup
    "\n ============= Test Fish script generation ============"
    [ testCase "basic fish comp" $
        GenFish.makeFishLineOption cmd opt @?= fishExpected,
      testCase "zsh script generation" $
        GenZsh.genZshScript cmd opts @?= zshScriptExpected,
      testCase "bash script generation" $
        GenBash.genBashScript cmd opts @?= bashScriptExpected,
      testCase "fish multi-level ancestor condition (root)" $
        GenFish.makeAncestorCondition ["mycmd"] @?= T.pack "",
      testCase "fish multi-level ancestor condition (1 level)" $
        GenFish.makeAncestorCondition ["mycmd", "sub1"] @?= T.pack "__fish_seen_subcommand_from sub1",
      testCase "fish multi-level ancestor condition (2 levels)" $
        GenFish.makeAncestorCondition ["mycmd", "sub1", "subsub1"] @?= T.pack "__fish_seen_subcommand_from sub1; and __fish_seen_subcommand_from subsub1",
      testCase "fish no child condition (empty)" $
        GenFish.makeNoChildCondition [] @?= T.pack "",
      testCase "fish no child condition (with children)" $
        GenFish.makeNoChildCondition [dummyCmd "child1", dummyCmd "child2"] @?= T.pack "not __fish_seen_subcommand_from child1 child2",
      -- Description quoting: zsh _arguments treats `\`, `[`, `]`, `'`
      -- specially inside the bracketed description segment.
      testCase "zsh: backslash in description is escaped" $
        let result = GenZsh.genZshScript "cmd" [Opt [OptName "-x" ShortType] "" "regex \\d+"]
         in T.isInfixOf "regex \\\\d+" result @?= True,
      testCase "zsh: brackets in description are escaped" $
        let result = GenZsh.genZshScript "cmd" [Opt [OptName "-x" ShortType] "" "list[items]"]
         in T.isInfixOf "list\\[items\\]" result @?= True,
      testCase "zsh: backslash before bracket in description is escape-ordered correctly" $
        -- Input "match \[end" must escape backslash first (-> "match \\[end")
        -- and then bracket (-> "match \\\[end"); reverse order would
        -- double-escape the inserted backslash.
        let result = GenZsh.genZshScript "cmd" [Opt [OptName "-x" ShortType] "" "match \\[end"]
         in T.isInfixOf "match \\\\\\[end" result @?= True
    ]
  where
    cmd = "nanachi"
    names = [OptName "-o" ShortType, OptName "--output" LongType]
    arg = "<file>"
    desc = "Specify the filename to save"
    opt = Opt names arg desc
    fishExpected = "complete -c nanachi -s \"o\" -l \"output\" -d \"Specify the filename to save\" -r"

    names2 = [OptName "--help" LongType]
    args2 = ""
    desc2 = "Help here."
    opt2 = Opt names2 args2 desc2
    opts = [opt, opt2]
    zshScriptExpected =
      "#compdef nanachi\n\n\
      \# Auto-generated with h2o\n\n\
      \args=(\n\
      \    {-o,--output}'[Specify the filename to save]':file:_files\n\
      \    '--help[Help here.]'\n\
      \)\n\n\
      \_arguments -s $args\n"
    bashScriptExpected =
      "# Auto-generated with h2o\n\
      \\n\
      \_nanachi()\n\
      \{\n\
      \    local cur prev words cword\n\
      \    _init_completion -s || return\n\
      \\n\
      \    local word_list=\"  -o --output --help\"\n\
      \    COMPREPLY=( $(compgen -W \"${word_list}\" -- \"$cur\") )\n\
      \}\n\n\
      \## -o bashdefault and -o default are fallback\n\
      \complete -o bashdefault -o default -F _nanachi nanachi\n"
    dummyCmd n = Command n [] "" "" [] [] ""
