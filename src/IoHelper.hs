{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IoHelper where

import Control.Exception (SomeException, try)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified System.Exit
import qualified System.Process.Typed as Process
import Text.Printf (printf)
import qualified Utils
import qualified Config

getManSub :: [String] -> IO Text
getManSub names = getMan $ List.intercalate "-" names

getManAndHelpSub :: [String] -> IO Text
getManAndHelpSub names = do
  content <- getManSub names
  if T.null content
    then do
      content2 <- getHelpSub names
      if T.null content2
        then error ("io: Neither help or man pages available: " ++ List.intercalate "-" names)
        else Utils.infoTrace "io: Using help for subcommand" $ return content2
    else Utils.infoTrace "io: Using manpage for subcommand" $ return content

-- |
getHelpTemplateMeta :: (Text -> Bool) -> String -> [[String]] -> IO Text
getHelpTemplateMeta _ _ [] = return ""
getHelpTemplateMeta isGood name (args : argsBag) = do
  emx <- try (fetchHelpInfo isGood name args) :: IO (Either SomeException (Maybe Text))
  case emx of
    Left _ -> return ""
    Right mx -> case mx of
      Just x -> return x
      Nothing -> getHelpTemplateMeta isGood name argsBag

-- | Get a CLI help page
getHelpTemplate :: String -> [[String]] -> IO Text
getHelpTemplate = getHelpTemplateMeta Utils.mayContainUseful

fetchHelpInfo :: (Text -> Bool) -> String -> [String] -> IO (Maybe Text)
fetchHelpInfo isGood name args = do
  (exitCode, stdout, stderr) <- Process.readProcess pc
  let stdoutText = TL.toStrict . TLE.decodeUtf8 $ stdout
  let stderrText = TL.toStrict . TLE.decodeUtf8 $ stderr
  let res
        | isCommandNotFound exitCode = Utils.warnTrace "CommandNotFound" Nothing
        | any Utils.hasErrorMessageAtTop [stdoutText, stderrText] =
          Utils.warnTrace ("The command seems invalid: " ++ unwords (name : args)) Nothing
        | isGood stdoutText = Utils.debugTrace ("Using stdout: " ++ unwords (name : args)) $ Just stdoutText
        | isGood stderrText = Utils.debugTrace ("Using stderr: " ++ unwords (name : args)) $ Just stderrText
        | otherwise = Nothing
  return res
  where
    pc = Process.shell $ unwords (name : args) ++ removeColorPostfix
    removeColorPostfix = " | sed -r 's/.\x08//g' | sed -r 's/\x1B\\[(([0-9]{1,2})?(;)?([0-9]{1,2})?)?[m,K,H,f,J]//g'"

isCommandNotFound :: System.Exit.ExitCode -> Bool
isCommandNotFound exitCode = exitCode == System.Exit.ExitFailure 127

getHelp :: String -> IO Text
getHelp name = getHelpTemplate name Config.helpOptions

getHelpSub :: [String] -> IO Text
getHelpSub names
  | null names = return T.empty
  | length names == 1 = getHelp (head names)
  | name == "bazel" = getHelpTemplate name [["help", subname, "--long"]]
  | otherwise = getHelpTemplate name (Config.helpOptionsSub subname)
  where
    name = unwords (init names)
    subname = last names

getMan :: String -> IO Text
getMan name = do
  (exitCode, stdout, _) <- Process.readProcess pc
  -- The exit code is actually thrown when piped to others...
  if exitCode == System.Exit.ExitFailure 16
    then return ""
    else return . TL.toStrict . TLE.decodeUtf8 $ stdout
  where
    pc = Process.shell $ printf "man %s | col -bx" name

getManAndHelp :: String -> IO Text
getManAndHelp name = do
  content <- getMan name
  if T.null content
    then do
      content2 <- getHelp name
      if T.null content2
        then error ("io: Neither help or man pages available: " ++ name)
        else Utils.infoTrace "io: Using help" $ return content2
    else Utils.infoTrace "io: Using manpage" $ return content

-- | Checks if man page is available
isManAvailableIO :: String -> IO Bool
isManAvailableIO name = do
  (exitCode, _, _) <- Process.readProcess pc
  -- The exit code is actually thrown when piped to others...
  return $ exitCode == System.Exit.ExitSuccess
  where
    pc = Process.shell $ printf "man -w %s" name

getVersion :: String -> IO Text
getVersion name = getHelpTemplateMeta Utils.isNotNullAndErrorMessageAbsent name [["--version"], ["version"], ["-version"]]
