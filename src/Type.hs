{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Type where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toEncoding, toJSON),
    object,
    pairs,
    withObject,
    (.:),
    (.:?),
  )
import qualified Data.List as List
import qualified Data.Aeson.Types as Aeson
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

data Command = Command
  { _name :: String, -- command name
    _aliases :: [String], -- command aliases
    _description :: String, -- description of command itself
    _usage :: String, -- usage
    _options :: [Opt], -- command options
    _subcommands :: [Command], -- subcommands
    _version :: String -- version
  }
  deriving (Eq, Show)

data Opt = Opt
  { _names :: [OptName],
    _arg :: String,
    _desc :: String
  }
  deriving (Eq)

data Subcommand = Subcommand
  { _name :: String,
    _aliases :: [String],
    _desc :: String
  }
  deriving (Eq)

instance Show Subcommand where
  show (Subcommand name aliases desc) = printf "%-25s (%s)" displayedNames desc
    where
      displayedNames = List.intercalate ", " (name : aliases)

instance Ord Subcommand where
  compare (Subcommand n1 _ _) (Subcommand n2 _ _) = compare n1 n2

data OptName = OptName
  { _raw :: String,
    _type :: OptNameType
  }
  deriving (Eq)

data OptNameType = LongType | ShortType | OldType | DoubleDashOnlyType | SingleDashOnlyType deriving (Eq, Show, Ord)

instance Show Opt where
  show (Opt names args desc) =
    printf "%s  ::  %s\n%s\n" (unwords (map _raw names)) args desc

instance Show OptName where
  show (OptName raw _) = show raw

instance Ord OptName where
  (OptName raw1 t1) `compare` (OptName raw2 t2) = (raw1, t1) `compare` (raw2, t2)

instance Ord Opt where
  Opt n1 a1 d1 `compare` Opt n2 a2 d2 = (n1, a1, d1) `compare` (n2, a2, d2)

instance ToJSON OptName where
  toJSON (OptName raw _) = toJSON raw
  toEncoding (OptName raw _) = toEncoding raw

instance FromJSON Opt where
  parseJSON = withObject "Opt" $ \v -> do
    rawNames <- v .: "names"
    when (null rawNames) $
      fail "'names' must be a non-empty array; at least one option name is required."
    names <- traverse parseOptName rawNames
    arg <- T.unpack <$> v .: "argument"
    desc <- T.unpack <$> v .: "description"
    return (Opt names arg desc)
    where
      parseOptName :: Text -> Aeson.Parser OptName
      parseOptName n = case toOptionNameType n of
        Just t -> return (OptName (T.unpack n) t)
        Nothing ->
          fail $
            "Invalid option name "
              ++ show (T.unpack n)
              ++ ". Each name must start with '-'."

instance FromJSON Command where
  parseJSON = withObject "Command" $ \v ->
    Command
      <$> (T.unpack <$> v .: "name")
      <*> (map T.unpack . Maybe.fromMaybe [] <$> v .:? "aliases")
      <*> (T.unpack <$> v .: "description")
      <*> (T.unpack . Maybe.fromMaybe "" <$> v .:? "usage")
      <*> v
      .: "options"
      <*> (Maybe.fromMaybe [] <$> v .:? "subcommands")
      <*> (T.unpack . Maybe.fromMaybe "" <$> v .:? "version")

instance ToJSON Opt where
  toJSON (Opt names arg desc) =
    object ["names" .= names, "argument" .= arg, "description" .= desc]

  toEncoding (Opt names arg desc) =
    pairs ("names" .= names <> "argument" .= arg <> "description" .= desc)

instance ToJSON Command where
  toJSON (Command name aliases desc usage opts subcommands version) =
    object $
      [ "name" .= name,
        "description" .= desc,
        "usage" .= usage,
        "options" .= opts
      ]
        ++ aliasesField
        ++ subcommandsField
        ++ versionField
    where
      aliasesField = ["aliases" .= aliases | not (null aliases)]
      subcommandsField = ["subcommands" .= subcommands | not (null subcommands)]
      versionField = ["version" .= version | not (null version)]

  toEncoding (Command name aliases desc usage opts subcommands version) =
    pairs $
      "name" .= name
        <> aliasesPair
        <> "description" .= desc
        <> "usage" .= usage
        <> "options" .= opts
        <> subcommandsPair
        <> versionPair
    where
      aliasesPair = if null aliases then mempty else "aliases" .= aliases
      subcommandsPair = if null subcommands then mempty else "subcommands" .= subcommands
      versionPair = if null version then mempty else "version" .= version

instance ToJSON Subcommand where
  toJSON (Subcommand name aliases desc) =
    object $
      ["name" .= name, "desc" .= desc]
        ++ ["aliases" .= aliases | not (null aliases)]

  toEncoding (Subcommand name aliases desc) =
    pairs ("name" .= name <> aliasesPair <> "desc" .= desc)
    where
      aliasesPair = if null aliases then mempty else "aliases" .= aliases

instance FromJSON Subcommand where
  parseJSON = withObject "Subcommand" $ \v ->
    Subcommand
      <$> (v .: "name" <|> v .: "cmd")  -- Accept both for backward compatibility
      <*> (Maybe.fromMaybe [] <$> v .:? "aliases")
      <*> v .: "desc"

-- | Classify an option name string. Returns 'Nothing' when the input does
-- not start with a dash (it is then not a recognisable option name and the
-- caller should treat it as a parse error rather than fabricate a type).
toOptionNameType :: Text -> Maybe OptNameType
toOptionNameType "-" = Just SingleDashOnlyType
toOptionNameType "--" = Just DoubleDashOnlyType
toOptionNameType s
  | "--" `T.isPrefixOf` s = Just LongType
  | "-" `T.isPrefixOf` s && T.length s == 2 = Just ShortType
  | "-" `T.isPrefixOf` s = Just OldType
  | otherwise = Nothing

asSubcommand :: Command -> Subcommand
asSubcommand (Command n aliases desc _ _ _ _) = Subcommand n aliases desc
