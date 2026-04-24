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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Exit (ExitCode (..), die)
import qualified System.Process.Typed as Process
import System.Timeout (timeout)
import qualified Utils

-- | Timeout (in microseconds) for any single external help/man invocation.
-- Prevents h2o from hanging on commands that wait on stdin or never return.
processTimeoutMicros :: Int
processTimeoutMicros = 10 * 1000 * 1000  -- 10 seconds

-- | Run a process with both a timeout and exception protection. Returns
-- 'Nothing' if the process binary cannot be started (e.g. @man@ is not
-- installed) or the process does not finish within 'processTimeoutMicros'.
-- The label is only used to annotate warning trace messages.
runProcessSafe ::
  String ->
  Process.ProcessConfig stdin stdout stderr ->
  IO (Maybe (ExitCode, BSL.ByteString, BSL.ByteString))
runProcessSafe label pc = do
  result <- try (timeout processTimeoutMicros (Process.readProcess pc))
  case result of
    Left (e :: SomeException) ->
      return $ Utils.warnTrace ("Cannot run " ++ label ++ ": " ++ show e) Nothing
    Right Nothing ->
      return $ Utils.warnTrace ("Timeout running: " ++ label) Nothing
    Right (Just r) -> return (Just r)

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
  result <- runProcessSafe (unwords cmdSeq) pc
  case result of
    Nothing -> return Nothing
    Just (exitCode, stdout, stderr) -> do
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
  result <- runProcessSafe ("man " ++ name) pc
  case result of
    Nothing -> return ""
    Just (exitCode, stdout, _)
      | exitCode /= ExitSuccess -> return ""
      | otherwise -> return . Utils.cleanTerminalOutput . TL.toStrict . TLE.decodeUtf8 $ stdout
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

-- | Checks if man page is available. Returns 'False' if the @man@ binary
-- is not installed, the lookup times out, or no man page exists for @name@.
isManAvailableIO :: String -> IO Bool
isManAvailableIO name = do
  result <- runProcessSafe ("man -w " ++ name) pc
  return $ case result of
    Nothing -> False
    Just (exitCode, _, _) -> exitCode == ExitSuccess
  where
    pc = Process.proc "man" ["-w", name]

getVersion :: String -> IO Text
getVersion name = getHelpTemplateMeta isGood name Config.versionOptions
  where
    isGood t =
      Utils.isNotNullAndErrorMessageAbsent t (T.pack name)
        && (not . Utils.isUsageBlock) t
