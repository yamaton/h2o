{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Type where

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
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

data Command = Command
  { _name :: String, -- command name
    _description :: String, -- description of command itself
    _options :: [Opt], -- command options
    _subcommands :: [Command], -- subcommands
    _version :: String -- version
  }
  deriving (Show)

data Opt = Opt
  { _names :: [OptName],
    _arg :: String,
    _desc :: String
  }
  deriving (Eq)

data Subcommand = Subcommand
  { _cmd :: String,
    _desc :: String
  }
  deriving (Eq, Ord)

instance Show Subcommand where
  show (Subcommand cmd desc) = printf "%-25s (%s)" cmd desc

data OptName = OptName
  { _raw :: String,
    _type :: OptNameType
  }
  deriving (Eq)

data OptNameType = LongType | ShortType | OldType | DoubleDashAlone | SingleDashAlone deriving (Eq, Show, Ord)

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
  parseJSON = withObject "Opt" $ \v ->
    Opt
      <$> (map toOptName <$> v .: "names")
      <*> (T.unpack <$> v .: "argument")
      <*> (T.unpack <$> v .: "description")
    where
      toOptName :: Text -> OptName
      toOptName n = OptName (T.unpack n) (toOptionNameType n)

instance FromJSON Command where
  parseJSON = withObject "Command" $ \v ->
    Command <$> (T.unpack <$> v .: "name")
      <*> (T.unpack <$> v .: "description")
      <*> v .: "options"
      <*> (Maybe.fromMaybe [] <$> v .:? "subcommands")
      <*> (T.unpack . Maybe.fromMaybe "" <$> v .:? "version")

instance ToJSON Opt where
  toJSON (Opt names arg desc) =
    object ["names" .= names, "argument" .= arg, "description" .= desc]

  toEncoding (Opt names arg desc) =
    pairs ("names" .= names <> "argument" .= arg <> "description" .= desc)

instance ToJSON Command where
  toJSON (Command name desc opts [] "") =
    object ["name" .= name, "description" .= desc, "options" .= opts]
  toJSON (Command name desc opts subcommands "") =
    object ["name" .= name, "description" .= desc, "options" .= opts, "subcommands" .= subcommands]
  toJSON (Command name desc opts [] version) =
    object ["name" .= name, "description" .= desc, "options" .= opts, "version" .= version]
  toJSON (Command name desc opts subcommands version) =
    object ["name" .= name, "description" .= desc, "options" .= opts, "subcommands" .= subcommands, "version" .= version]

  toEncoding (Command name desc opts [] "") =
    pairs ("name" .= name <> "description" .= desc <> "options" .= opts)
  toEncoding (Command name desc opts subcommands "") =
    pairs ("name" .= name <> "description" .= desc <> "options" .= opts <> "subcommands" .= subcommands)
  toEncoding (Command name desc opts [] version) =
    pairs ("name" .= name <> "description" .= desc <> "options" .= opts <> "version" .= version)
  toEncoding (Command name desc opts subcommands version) =
    pairs ("name" .= name <> "description" .= desc <> "options" .= opts <> "subcommands" .= subcommands <> "version" .= version)

toOptionNameType :: Text -> OptNameType
toOptionNameType "-" = SingleDashAlone
toOptionNameType "--" = DoubleDashAlone
toOptionNameType s
  | "--" `T.isPrefixOf` s = LongType
  | "-" `T.isPrefixOf` s && T.length s == 2 = ShortType
  | "-" `T.isPrefixOf` s = OldType
  | otherwise = error "Invalid option name!"

asSubcommand :: Command -> Subcommand
asSubcommand (Command n desc _ _ _) = Subcommand n desc
