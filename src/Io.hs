{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Io where

import CommandArgs (Config (..), ConfigOrVersion (..), Input (..), OutputFormat (..), defaultSubprocessBudget)
import Control.Exception (throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Ordered as OMap
import Data.Text (Text)
import qualified Data.Text as T
import GenBashCompletions (toBashScript)
import GenFishCompletions (toFishScript)
import GenJSON (toJSONText)
import GenZshCompletions (toZshScript)
import H2OError (H2OError (..))
import IoHelper
  ( getHelp,
    getHelpSub,
    getManAndHelp,
    getManAndHelpSub,
    getManThenHelpSub,
    isManAvailableIO,
  )
import Layout (parseBlockwise, parseUsage, preprocessBlockwise)
import qualified Postprocess
import Subcommand (parseSubcommand)
import System.FilePath (takeBaseName)
import Text.Printf (printf)
import Type (Command (..), Opt, Subcommand (..), asSubcommand)
import qualified Utils
import qualified Version

-- | Main function processing ConfigOrVersion
run :: ConfigOrVersion -> IO Text
-- Just return version
run Version = return (T.concat ["h2o ", Version.versionStr, "\n"])
-- Or, do some utility work
run (C_ (Config input _ isExportingJSON isListingSubcommands isPreprocessOnly depth subprocessLimit _))
  | isExportingJSON =
      Utils.warnTrace "Deprecated: --json flag. Use --format json instead" $
        run (C_ (Config input Json False False False depth subprocessLimit False))
  | isListingSubcommands =
      Utils.infoTrace "Listing subcommands..." $
        T.unlines <$> listSubcommandsIO input depth subprocessLimit
  | isPreprocessOnly =
      Utils.infoTrace "Running preprocessing only (splitting options and descriptions)" $
        T.pack . formatStringPairs . preprocessBlockwise <$> getInputContent input
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])

-- Or, process the input file in text
run (C_ (Config input@(FileInput f skipMan) format _ _ _ depth subprocessLimit _)) =
  toScript format <$> (pageToCommandIO name skipMan depth subprocessLimit False =<< contentIO)
  where
    name = takeBaseName f
    contentIO = getInputContent input

-- Or, process with command name
run (C_ (Config input@(CommandInput name skipMan) format _ _ _ depth subprocessLimit _)) =
  toScript format <$> (pageToCommandIO name skipMan depth subprocessLimit True =<< contentIO)
  where
    contentIO = getInputContent input

-- Or, process with command name AND subcommand name
run (C_ (Config input@(SubcommandInput name subname _) format _ _ _ _ _ _)) =
  toScript format <$> (pageToCommandSimple nameSubname True =<< getInputContent input)
  where
    nameSubname = name ++ "-" ++ subname

-- Or, load Command from JSON
run (C_ (Config (JsonInput f) format _ _ _ _ _ _)) = do
  content <- BSL.readFile f
  case Aeson.eitherDecode content :: Either String Command of
    Left err -> throwIO (JsonDecodeFailed f err)
    Right c -> toScript format <$> return c

toOptsText :: [Opt] -> Text
toOptsText = T.unlines . map (T.pack . show)

toSubcommandsText :: [String] -> [Command] -> Text
toSubcommandsText path cmds =
  if T.null main then T.empty else prefix `T.append` main
  where
    prefix =
      if null path
        then T.empty
        else T.pack . printf "(%s)\n" . T.unwords . map T.pack $ path
    main = T.unlines . map (T.pack . show . asSubcommand) $ cmds

toSubcommandOptionsText :: [String] -> Command -> Text
toSubcommandOptionsText nameSeq (Command subname _ _ _ opts _ _) =
  T.unlines $ map (\opt -> prefix `T.append` T.pack (show opt)) opts
  where
    prefix = T.pack . printf "(%s) " . T.unwords . map T.pack $ nameSeq ++ [subname]

normalizeInputText :: Text -> Text
normalizeInputText = Utils.maskListBullets . Utils.unicodeSpacesToAscii . Utils.convertTabsToSpaces 8

readTextFileLenient :: FilePath -> IO Text
readTextFileLenient f = Utils.decodeUtf8Lenient <$> BSL.readFile f

getInputContent :: Input -> IO String
getInputContent (SubcommandInput name subname skipMan) =
  T.unpack . normalizeInputText <$> reader [name, subname]
  where
    reader = if skipMan then getHelpSub else getManAndHelpSub
getInputContent (CommandInput name skipMan) =
  T.unpack . normalizeInputText <$> reader name
  where
    reader = if skipMan then getHelp else getManAndHelp
getInputContent (FileInput f _) =
  T.unpack . normalizeInputText <$> readTextFileLenient f
getInputContent (JsonInput f) = T.unpack <$> readTextFileLenient f

toScript :: OutputFormat -> Command -> Text
toScript Fish = toFishScript
toScript Zsh = toZshScript
toScript Bash = toBashScript
toScript Json = toJSONText
toScript Native = toNativeText

toNativeText :: Command -> Text
toNativeText cmd =
  T.intercalate "\n\n\n" . filter (not . T.null) $ toNativeTextRec [] cmd

toNativeTextRec :: [String] -> Command -> [Text]
toNativeTextRec path cmd@(Command name _ desc usage _ subCmds _) =
  [nameText, descText, usageText, optsText, subcommandsText] ++ rest
  where
    currentPath = path ++ [name]
    nameText = "Name:  " `T.append` T.intercalate " " (map T.pack currentPath)
    descText = "Desc:  " `T.append` T.pack desc
    usageText = "Usage:\n" `T.append` T.pack usage
    optsText = toSubcommandOptionsText path cmd
    subcommandsText = toSubcommandsText currentPath subCmds
    rest = concatMap (toNativeTextRec currentPath) subCmds

-- | Maximum subcommand candidates considered at any single recursion level.
-- Acts as a noise filter: when the parser overestimates how many
-- subcommands a help text contains, we cap the explosion at one level
-- before it cascades through the rest of the tree.
maxSubcandidatesPerLevel :: Int
maxSubcandidatesPerLevel = 100

-- | Scans over command and subcommands
--
-- `name` is the name of the command.
-- `skipMan` sets whether to read man pages in subsequent scans.
-- `fetchVersion` controls whether to shell out to @\<name\> --version@ to
--     populate the 'Command._version' field. Set 'False' for inputs where
--     the command is not installed (file / JSON input); the lookup would
--     fail anyway and each failed invocation costs one slot of
--     the subprocess budget and up to 'processTimeoutMicros' of wall time.
-- `content` is the top-level text to be scanned.
pageToCommandIO :: String -> Bool -> Int -> Int -> Bool -> String -> IO Command
pageToCommandIO name skipMan depth subprocessLimit fetchVersion content = do
  isManAvailable <- isManAvailableIO name
  let useMan = not skipMan && isManAvailable
  budget <- newIORef subprocessLimit
  (cmd, status) <- getCommandRec budget subprocessLimit depth useMan [name] [] name "placeholder" content
  let isSuccess =
        (not . null . _options) cmd
          || (not . null . _subcommands) cmd
          || (not . null . _usage) cmd
  if status && isSuccess
    then postProcess cmd
    else throwIO (NoExtractableOptions name)
  where
    postProcess
      | fetchVersion = Postprocess.fixCommand
      | otherwise    = return . Postprocess.fixOpts

-- | Scan subcommand recursively for its options and sub-sub commands
--
-- Arguments:
--   budget is a shared IORef counting remaining subprocess invocations.
--     When it reaches zero, scanning aborts instead of emitting incomplete
--     output.
--   extraDepth is the number of extra depths to scan sub-sub..commands. Set 0 to avoid scanning sub-sub commands.
--   useMan carries information whether man page should be tried first.
--     When True, each subcommand fetch tries the man page and falls back
--     to --help if no man page exists for that particular subcommand.
--     When False (--skip-man), help is used directly without trying man.
--   cmdSeq is a list composed of command name, subcommand name, for example ["docker", "container", "run"].
--   aliases are aliases for the command at the end of cmdSeq.
--   desc is description of the subcommand obtained from the upper-level source.
--   upperContent is the text scanned in the upper level. This information is needed because
--     "foo bar --help" sometimes returns the identical result as "foo --help".
getCommandRec :: IORef Int -> Int -> Int -> Bool -> [String] -> [String] -> String -> Text -> String -> IO (Command, Bool)
getCommandRec budget budgetLimit extraDepth useMan cmdSeq aliases desc upperContent givenPage = do
  page <-
    if null givenPage
      then do
        remaining <- readIORef budget
        if remaining <= 0
          then throwIO (SubprocessBudgetExhausted (unwords cmdSeq) budgetLimit)
          else do
            modifyIORef' budget (subtract 1)
            normalizeInputText <$> readFunc cmdSeq
      else return (T.pack givenPage)
  let content = T.unpack page
  let isSuccess = not (T.null page) && page /= upperContent
  let subCandidates =
        if extraDepth <= 0
          then []
          else take maxSubcandidatesPerLevel (getSubcmdCandidates content)
  let subCommandCandidsM =
        mapM
          ( \(Subcommand name subAliases subDesc) ->
              getCommandRec budget budgetLimit (extraDepth - 1) useMan (cmdSeq ++ [name]) subAliases subDesc page ""
          )
          ( filter
              (\(Subcommand name _ _) -> null cmdSeq || (last cmdSeq /= name))
              subCandidates
          )
  let subCommandsM = map fst . filter snd <$> subCommandCandidsM
  let opts = parseBlockwise content
  subCommands <- subCommandsM
  let usage = parseUsage content
  let result = Command (last cmdSeq) aliases desc usage opts subCommands ""
  return (result, Utils.infoMsg ("Extraction succeeded for " ++ unwords cmdSeq ++ ":") isSuccess)
  where
    readFunc = if useMan then getManThenHelpSub else getHelpSub

-- | scan `content` for a list of possible subcommand
getSubcmdCandidates :: String -> [Subcommand]
getSubcmdCandidates content =
  Utils.infoMsg "Subcommand candidates:" $
    uniqSubcommands . parseSubcommand $
      content
  where
    sub2pair (Subcommand n aliases d) = (n, (aliases, d))
    pair2sub (n, (aliases, d)) = Subcommand n aliases d
    uniqSubcommands = map pair2sub . OMap.assocs . OMap.fromList . map sub2pair

-- | Converts to Command given command name and text. See 'pageToCommandIO'
-- for the 'fetchVersion' semantics.
pageToCommandSimple :: String -> Bool -> String -> IO Command
pageToCommandSimple name fetchVersion content =
  if null rootOptions
    then throwIO (NoExtractableOptions name)
    else postProcess $ Command name [] name usage rootOptions [] ""
  where
    rootOptions = parseBlockwise content
    usage = parseUsage content
    postProcess
      | fetchVersion = Postprocess.fixCommand
      | otherwise    = return . Postprocess.fixOpts

listSubcommandsIO :: Input -> Int -> Int -> IO [Text]
listSubcommandsIO input depth subprocessLimit =
  formatSubcommandTree . _subcommands
    <$> (pageToCommandIO name skipMan depth subprocessLimit False =<< getInputContent input)
  where
    name = getName input
    skipMan = getSkipMan input

formatSubcommandTree :: [Command] -> [Text]
formatSubcommandTree = go 0
  where
    go level = concatMap (formatOne level)
    formatOne level cmd@(Command _ _ _ _ _ subcommands _) =
      (T.replicate level "  " <> toListedSubcommandText cmd) : go (level + 1) subcommands

toListedSubcommandText :: Command -> Text
toListedSubcommandText = T.pack . show . asSubcommand

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
