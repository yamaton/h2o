{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Io where

import CommandArgs (Config (..), ConfigOrVersion (..), Input (..), OutputFormat (..))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Ordered as OMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GenBashCompletions (toBashScript)
import GenFishCompletions (toFishScript)
import GenJSON (toJSONText)
import GenZshCompletions (toZshScript)
import IoHelper
  ( getHelp,
    getHelpSub,
    getManAndHelp,
    getManSub,
    isManAvailableIO,
  )
import Layout (parseBlockwise, preprocessBlockwise, parseUsage)
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
run (C_ (Config input _ isExportingJSON isPreprocessOnly depth))
  | isExportingJSON =
    Utils.warnTrace "io: Deprecated: Use --format json instead" $
      run (C_ (Config input Json False False depth))
  | isPreprocessOnly =
    Utils.infoTrace "io: processing (option+arg, description) splitting only" $
      T.pack . formatStringPairs . preprocessBlockwise <$> getInputContent input
  where
    formatStringPairs = unlines . map (\(a, b) -> unlines [a, b])

-- Or, process the input file in text
run (C_ (Config input@(FileInput f skipMan) format _ _ depth)) =
  toScript format <$> (pageToCommandIO name skipMan depth =<< contentIO)
  where
    name = takeBaseName f
    contentIO = getInputContent input

-- Or, process with command name
run (C_ (Config input@(CommandInput name skipMan) format _ _ depth)) =
  toScript format <$> (pageToCommandIO name skipMan depth =<< contentIO)
  where
    contentIO = getInputContent input

-- Or, load Command from JSON
run (C_ (Config input@(JsonInput _) format _ _ _)) = do
  content <- TLE.encodeUtf8 . TL.pack <$> getInputContent input
  let cmdMay = Aeson.decode content :: Maybe Command
  let commandIO =
        case cmdMay of
          Nothing -> error "Cannot decode JSON!"
          Just c -> return c
  toScript format <$> commandIO

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
toSubcommandOptionsText nameSeq (Command subname _ _ opts _ _) =
  T.unlines $ map (\opt -> prefix `T.append` T.pack (show opt)) opts
  where
    prefix = T.pack . printf "(%s) " . T.unwords . map T.pack $ nameSeq ++ [subname]

getInputContent :: Input -> IO String
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
toNativeText (Command _ _ _ opts [] _) =
  Utils.warnTrace "No subcommands" $
    T.unlines $ map (T.pack . show) opts
toNativeText cmd =
  T.intercalate "\n\n\n" . filter (not . T.null) $ toNativeTextRec [] cmd

toNativeTextRec :: [String] -> Command -> [Text]
toNativeTextRec path cmd@(Command name _ _ _ subCmds _) =
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
  isManAvailable <- isManAvailableIO name
  let useMan = not skipMan && isManAvailable
  (cmd, status) <- getCommandRec depth useMan [name] name "placeholder" content
  if status && ((not . null . _options) cmd || (not . null . _subcommands) cmd)
    then Postprocess.fixCommand cmd
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
  page <-
    if null givenPage
      then Utils.convertTabsToSpaces 8 <$> readFunc cmdSeq
      else return (T.pack givenPage)
  let content = T.unpack page
  let isSuccess = not (T.null page) && page /= upperContent
  let subCandidates = if extraDepth <= 0 then [] else getSubcmdCandidates content
  let subCommandCandidsM =
        mapM
          ( \(Subcommand subName subDesc) ->
              getCommandRec (extraDepth - 1) useMan (cmdSeq ++ [subName]) subDesc page ""
          )
          subCandidates
  let subCommandsM = map fst . filter snd <$> subCommandCandidsM
  let opts = parseBlockwise content
  subCommands <- subCommandsM
  let usage = parseUsage content
  let result = Command (last cmdSeq) desc usage opts subCommands ""
  return (result, Utils.infoMsg ("getCommandRec isSuccess: " ++ unwords cmdSeq) isSuccess)
  where
    readFunc = if useMan then getManSub else getHelpSub

-- | scan `content` for a list of possible subcommand
getSubcmdCandidates :: String -> [Subcommand]
getSubcmdCandidates content =
  Utils.infoMsg "subcommand candidates: \n" $
    removeHelp . uniqSubcommands . parseSubcommand $ content
  where
    sub2pair (Subcommand s1 s2) = (s1, s2)
    pair2sub = uncurry Subcommand
    uniqSubcommands = map pair2sub . OMap.assocs . OMap.fromList . map sub2pair
    removeHelp = filter (\(Subcommand name _) -> name /= "help")

-- | Converts to Command given command name and text
pageToCommandSimple :: String -> String -> IO Command
pageToCommandSimple name content =
  if null rootOptions
    then error ("Failed to extract information for a Command: " ++ name)
    else Postprocess.fixCommand $ Command name name usage rootOptions [] ""
  where
    rootOptions = parseBlockwise content
    usage = parseUsage content

listSubcommandsIO :: Input -> IO [Text]
listSubcommandsIO input = getSubnames <$> (pageToCommandIO name skipMan 1 =<< getInputContent input)
  where
    name = getName input
    skipMan = getSkipMan input
    getSubnames = map (T.pack . _name) . _subcommands

getName :: Input -> String
getName (CommandInput n _) = n
getName (FileInput f _) = takeBaseName f
getName (JsonInput f) = takeBaseName f

getSkipMan :: Input -> Bool
getSkipMan (CommandInput _ b) = b
getSkipMan (FileInput _ b) = b
getSkipMan (JsonInput _) = True
