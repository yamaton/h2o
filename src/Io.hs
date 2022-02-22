{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Io where

import CommandArgs (Config (..), ConfigOrVersion (..), Input (..), OutputFormat (..))
import qualified Constants
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Map.Ordered as OMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GenBashCompletions (toBashScript)
import GenFishCompletions (toFishScript)
import GenJSON (toJSONText)
import GenZshCompletions (toZshScript)
import Layout (parseBlockwise, preprocessBlockwise)
import qualified Postprocess
import Subcommand (parseSubcommand)
import qualified System.Exit
import System.FilePath (takeBaseName)
import qualified System.Process.Typed as Process
import Text.Printf (printf)
import Type (Command (..), Opt, Subcommand (..), asSubcommand)
import Utils (convertTabsToSpaces, infoMsg, infoTrace, mayContainUseful, warnMsg, warnTrace)
import qualified Utils

-- | Main function processing ConfigOrVersion
run :: ConfigOrVersion -> IO Text
-- Just return version
run Version = return (T.concat ["h2o ", Constants.versionStr, "\n"])
-- Or, do some utility work
run (C_ (Config input _ isExportingJSON isConvertingTabsToSpaces isListingSubcommands isPreprocessOnly depth))
  | isExportingJSON = Utils.warnTrace "io: Deprecated: Use --format json instead" $ run (C_ (Config input Json False False False False depth))
  | isConvertingTabsToSpaces = infoTrace "io: Converting tags to spaces...\n" T.pack <$> getInputContent input
  | isListingSubcommands = infoTrace "io: Listing subcommands...\n" $ T.unlines <$> listSubcommandsIO input
  | isPreprocessOnly = infoTrace "io: processing (option+arg, description) splitting only" $ T.pack . formatStringPairs . preprocessBlockwise <$> getInputContent input
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])

-- Or, process the input file in text
run (C_ (Config input@(FileInput f skipMan) format _ _ _ _ depth)) =
  toScript format <$> (pageToCommandIO name skipMan depth =<< contentIO)
  where
    name = takeBaseName f
    contentIO = getInputContent input

-- Or, process with command name
run (C_ (Config input@(CommandInput name skipMan) format _ _ _ _ depth)) =
  toScript format <$> (pageToCommandIO name skipMan depth =<< contentIO)
  where
    contentIO = getInputContent input

-- Or, process with command name AND subcommand name
run (C_ (Config input@(SubcommandInput name subname _) format _ _ _ _ _)) =
  toScript format . pageToCommandSimple nameSubname <$> getInputContent input
  where
    nameSubname = name ++ "-" ++ subname

-- Or, load Command from JSON
run (C_ (Config input@(JsonInput _) format _ _ _ _ _)) = do
  content <- TLE.encodeUtf8 . TL.pack <$> getInputContent input
  let cmdMay = Aeson.decode content :: Maybe Command
  let commandIO =
        case cmdMay of
          Nothing -> error "Cannot decode JSON!"
          Just c -> return c
  toScript format <$> commandIO

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
        else infoTrace "io: Using help for subcommand" $ return content2
    else infoTrace "io: Using manpage for subcommand" $ return content

-- |
getHelpTemplate :: String -> [[String]] -> IO Text
getHelpTemplate _ [] = return ""
getHelpTemplate name (args : argsBag) = do
  emx <- try (fetchHelpInfo name args) :: IO (Either SomeException (Maybe Text))
  case emx of
    Left _ -> return ""
    Right mx -> case mx of
      Just x -> return x
      Nothing -> getHelpTemplate name argsBag

fetchHelpInfo :: String -> [String] -> IO (Maybe Text)
fetchHelpInfo name args = do
  (exitCode, stdout, stderr) <- Process.readProcess pc
  let stdoutText = TL.toStrict . TLE.decodeUtf8 $ stdout
  let stderrText = TL.toStrict . TLE.decodeUtf8 $ stderr
  let res
        | isCommandNotFound name exitCode stderrText = warnMsg "CommandNotFound" Nothing
        | mayContainUseful stdoutText = Utils.debugTrace ("Using stdout: " ++ unwords (name : args)) $ Just stdoutText
        | mayContainUseful stderrText = Utils.debugTrace ("Using stderr: " ++ unwords (name : args)) $ Just stderrText
        | otherwise = Nothing
  return res
  where
    pc = Process.shell $ unwords (name : args) ++ removeColorPostfix
    removeColorPostfix = " | sed -r 's/.\x08//g' | sed -r 's/\x1B\\[(([0-9]{1,2})?(;)?([0-9]{1,2})?)?[m,K,H,f,J]//g'"

isCommandNotFound :: String -> System.Exit.ExitCode -> Text -> Bool
isCommandNotFound _ exitCode _ =
  exitCode == System.Exit.ExitFailure 127

getHelp :: String -> IO Text
getHelp name = getHelpTemplate name [["--help"], ["help"], ["-help"], ["-h"]]

getHelpSub :: [String] -> IO Text
getHelpSub names = getHelpTemplate name [[subname, "--help"], ["help", subname], [subname, "-help"], [subname, "-h"]]
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
        else infoTrace "io: Using help" $ return content2
    else infoTrace "io: Using manpage" $ return content

toOptsText :: [Opt] -> Text
toOptsText = T.unlines . map (T.pack . show)

toSubcommandsText :: [String] -> [Command] -> Text
toSubcommandsText path cmds =
  if T.null main then T.empty else prefix `T.append` main
  where
    prefix = if null path
      then T.empty
      else T.pack . printf "(%s)\n" . T.unwords . map T.pack $ path
    main = T.unlines . map (T.pack . show . asSubcommand) $ cmds

toSubcommandOptionsText :: [String] -> Command -> Text
toSubcommandOptionsText nameSeq (Command subname _ opts _) =
  T.unlines $ map (\opt -> prefix `T.append` T.pack (show opt)) opts
  where
    prefix = T.pack . printf "(%s) " . T.unwords . map T.pack $ nameSeq ++ [subname]

getInputContent :: Input -> IO String
getInputContent (SubcommandInput name subname skipMan) =
  T.unpack . Utils.convertTabsToSpaces 8 <$> reader [name, subname]
  where
    reader = if skipMan then getHelpSub else getManAndHelpSub
getInputContent (CommandInput name skipMan) =
  T.unpack . Utils.convertTabsToSpaces 8 <$> reader name
  where
    reader = if skipMan then getHelp else getManAndHelp
getInputContent (FileInput f _) =
  T.unpack . Utils.convertTabsToSpaces 8 . T.pack <$> readFile f
getInputContent (JsonInput f) = readFile f

toScript :: OutputFormat -> Command -> Text
toScript Fish = toFishScript
toScript Zsh = toZshScript
toScript Bash = toBashScript
toScript Json = toJSONText
toScript Native = toNativeText

toNativeText :: Command -> Text
toNativeText (Command _ _ opts []) = warnTrace "No subcommands" $ T.unlines $ map (T.pack .show) opts
toNativeText cmd = T.intercalate "\n\n\n" . filter (not . T.null) $ toNativeTextRec [] cmd

toNativeTextRec :: [String] -> Command -> [Text]
toNativeTextRec path cmd@(Command name _ _ subCmds) =
  [optsText, subcommandsText] ++ rest
  where
    optsText = toSubcommandOptionsText path cmd
    subcommandsText = toSubcommandsText (path ++ [name]) subCmds
    rest = concatMap (toNativeTextRec (path ++ [name])) subCmds

-- | Scans over command and subcommands
--
-- `name` is the name of the command.
-- `skipMan` sets weather to read man pages in subsequent scans.
-- `content` is the top-level text to be scanned.
pageToCommandIO :: String -> Bool -> Int -> String -> IO Command
pageToCommandIO name skipMan depth content = do
  !isManAvailable <- isManAvailableIO name
  let useMan = not skipMan && isManAvailable
  (cmd, status) <- getCommandRec depth useMan [name] name "placeholder" content
  if status && ((not . null . _options) cmd || (not . null. _subcommands) cmd)
    then return $ Postprocess.fixCommand cmd
    else error ("Failed to extract information for a Command: " ++ name)


-- | Scan subcommand recursively for its options and sub-sub commands
--
-- Arguments:
--   extraDepth is the number of extra depths to scan sub-sub..commands. Set 0 to avoid scanning sub-sub commands.
--   useMan carries information weather man page is used as the information source.
--   cmdSeq is a list composed of command name, subcommand name, for example ["docker", "container", "run"].
--   desc is description of the subcommand obtained from the upper-level source.
--   upperContent is the text scanned in the upper level. This information is needed because
--     "foo bar --help" sometimes returns the identical result as "foo --help".
getCommandRec :: Int -> Bool -> [String] -> String -> Text -> String -> IO (Command, Bool)
getCommandRec extraDepth useMan cmdSeq desc upperContent givenPage = do
  page <- if null givenPage
            then Utils.convertTabsToSpaces 8 <$> readFunc cmdSeq
            else return (T.pack givenPage)
  let content = T.unpack page
  let isSuccess = not (T.null page) && page /= upperContent
  let subCandidates = if extraDepth <= 0 then [] else getSubcmdCandidates content
  let subCommandCandidsM =
        mapM
        (\(Subcommand subName subDesc) ->
          getCommandRec (extraDepth - 1) useMan (cmdSeq ++ [subName]) subDesc page "")
        subCandidates
  let subCommandsM = map fst . filter snd <$> subCommandCandidsM
  let opts = parseBlockwise content
  subCommands <- subCommandsM
  let result = Command (last cmdSeq) desc opts subCommands
  return (result, infoMsg ("getCommandRec isSuccess: " ++ unwords cmdSeq) isSuccess)
  where
    readFunc = if useMan then getManSub else getHelpSub


-- | scan `content` for a list of possible subcommand
getSubcmdCandidates :: String -> [Subcommand]
getSubcmdCandidates content =
  infoMsg "subcommand candidates: \n" $ removeHelp . uniqSubcommands . parseSubcommand $ content
  where
    sub2pair (Subcommand s1 s2) = (s1, s2)
    pair2sub = uncurry Subcommand
    uniqSubcommands = map pair2sub . OMap.assocs . OMap.fromList . map sub2pair
    removeHelp = filter (\(Subcommand name _) -> name /= "help")

-- | Checks if man page is available
isManAvailableIO :: String -> IO Bool
isManAvailableIO name = do
  (exitCode, _, _) <- Process.readProcess pc
  -- The exit code is actually thrown when piped to others...
  return $ exitCode == System.Exit.ExitSuccess
  where
    pc = Process.shell $ printf "man -w %s" name

-- | Converts to Command given command name and text
pageToCommandSimple :: String -> String -> Command
pageToCommandSimple name content =
  if null rootOptions
    then error ("Failed to extract information for a Command: " ++ name)
    else Postprocess.fixCommand $ Command name name rootOptions []
  where
    rootOptions = parseBlockwise content

listSubcommandsIO :: Input -> IO [Text]
listSubcommandsIO input = getSubnames <$> (pageToCommandIO name skipMan 1 =<< getInputContent input)
  where
    name = getName input
    skipMan = getSkipMan input
    getSubnames = map (T.pack . _name) . _subcommands

getName :: Input -> String
getName (CommandInput n _) = n
getName (SubcommandInput n _ _) = n
getName (FileInput f _) = takeBaseName f
getName (JsonInput f) = takeBaseName f

getSkipMan :: Input -> Bool
getSkipMan (CommandInput _ b) = b
getSkipMan (SubcommandInput _ _ b) = b
getSkipMan (FileInput _ b) = b
getSkipMan (JsonInput _) = True
