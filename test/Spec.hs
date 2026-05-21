module Main (main) where

import System.Environment (setEnv)
import Test.Tasty (defaultMain, testGroup)

import qualified Test.FrameworkFixtureTests
import qualified Test.GoldenTests
import qualified Test.HelpParser.OptNameTests
import qualified Test.HelpParser.OptPartTests
import qualified Test.LayoutTests
import qualified Test.PropertyTests
import qualified Test.ShellCompletionTests
import qualified Test.UtilsTests

main :: IO ()
main = do
  setEnv "COLUMNS" "1000"
  defaultMain $
    testGroup
      "Tests"
      [ Test.HelpParser.OptNameTests.tests
      , Test.HelpParser.OptPartTests.tests
      , Test.LayoutTests.tests
      , Test.PropertyTests.tests
      , Test.ShellCompletionTests.tests
      , Test.FrameworkFixtureTests.tests
      , Test.GoldenTests.tests
      , Test.UtilsTests.tests
      ]
