{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IoHelper
  ( getHelp,
    getHelpSub,
    getMan,
    getManSub,
    getManAndHelp,
    getManAndHelpSub,
    isManAvailableIO,
    getVersion,
  )
where

import qualified Config
import Control.Exception (SomeException, try)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Exit (ExitCode (..), die)
import qualified System.Process.Typed as Process
import qualified Utils

getManSub :: [String] -> IO Text
getManSub names = getMan $ List.intercalate "-" names

getManAndHelpSub :: [String] -> IO Text
getManAndHelpSub names = do
  content <- getManSub names
  if T.null content
    then do
      content2 <- getHelpSub names
      if T.null content2
        then die $ "Error: No help or man page found for '" ++ List.intercalate " " names ++ "'. Is the command installed?"
        else Utils.infoTrace "Using help text for subcommand" $ return content2
    else Utils.infoTrace "Using man page for subcommand" $ return content

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
getHelpTemplate cmd = getHelpTemplateMeta (Utils.mayContainUseful (T.pack cmd)) cmd

fetchHelpInfo :: (Text -> Bool) -> String -> [String] -> IO (Maybe Text)
fetchHelpInfo isGood name args = do
  (exitCode, stdout, stderr) <- Process.readProcess pc
  let stdoutText = Utils.cleanTerminalOutput . TL.toStrict . TLE.decodeUtf8 $ stdout
  let stderrText = Utils.cleanTerminalOutput . TL.toStrict . TLE.decodeUtf8 $ stderr
  let res
        | isCommandNotFound exitCode = Utils.warnTrace "Command not found" Nothing
        | any (Utils.hasErrorMessageAtTop (T.pack name)) [stdoutText, stderrText] =
            Utils.warnTrace ("Command appears invalid: " ++ unwords cmdSeq) Nothing
        | isGood stdoutText = Utils.debugTrace ("Using stdout from: " ++ unwords cmdSeq) $ Just stdoutText
        | isGood stderrText = Utils.debugTrace ("Using stderr from: " ++ unwords cmdSeq) $ Just stderrText
        | otherwise = Nothing
  return res
  where
    cmdSeq = name : args
    cmdWords = words name ++ filter (not . all (== ' ')) args
    pc = Process.proc (head cmdWords) (tail cmdWords)

isCommandNotFound :: ExitCode -> Bool
isCommandNotFound exitCode = exitCode == ExitFailure 127

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
  if exitCode /= ExitSuccess
    then return ""
    else return . Utils.cleanTerminalOutput . TL.toStrict . TLE.decodeUtf8 $ stdout
  where
    pc = Process.proc "man" [name]

getManAndHelp :: String -> IO Text
getManAndHelp name = do
  content <- getMan name
  if T.null content
    then do
      content2 <- getHelp name
      if T.null content2
        then die $ "Error: No help or man page found for '" ++ name ++ "'. Is the command installed?"
        else Utils.infoTrace "Using help text" $ return content2
    else Utils.infoTrace "Using man page" $ return content

-- | Checks if man page is available
isManAvailableIO :: String -> IO Bool
isManAvailableIO name = do
  (exitCode, _, _) <- Process.readProcess pc
  return $ exitCode == ExitSuccess
  where
    pc = Process.proc "man" ["-w", name]

getVersion :: String -> IO Text
getVersion name = getHelpTemplateMeta isGood name Config.versionOptions
  where
    isGood t =
      Utils.isNotNullAndErrorMessageAbsent t (T.pack name)
        && (not . Utils.isUsageBlock) t
