{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Type where

import Control.Applicative ((<|>))
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
import qualified Data.Aeson.Types as Aeson
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

data Command = Command
  { _name :: String, -- command name
    _description :: String, -- description of command itself
    _usage :: String, -- usage
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
  { _name :: String,
    _desc :: String
  }
  deriving (Eq)

instance Show Subcommand where
  show (Subcommand name desc) = printf "%-25s (%s)" name desc

instance Ord Subcommand where
  compare (Subcommand n1 _) (Subcommand n2 _) = compare n1 n2

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
  toJSON (Command name desc usage opts [] "" ) =
    object ["name" .= name, "description" .= desc, "usage" .= usage, "options" .= opts]
  toJSON (Command name desc usage opts subcommands "") =
    object ["name" .= name, "description" .= desc, "usage" .= usage, "options" .= opts, "subcommands" .= subcommands]
  toJSON (Command name desc usage opts [] version) =
    object ["name" .= name, "description" .= desc, "usage" .= usage, "options" .= opts, "version" .= version]
  toJSON (Command name desc usage opts subcommands version) =
    object ["name" .= name, "description" .= desc, "usage" .= usage, "options" .= opts, "subcommands" .= subcommands, "version" .= version]

  toEncoding (Command name desc usage opts [] "") =
    pairs ("name" .= name <> "description" .= desc <> "usage" .= usage <> "options" .= opts)
  toEncoding (Command name desc usage opts subcommands "") =
    pairs ("name" .= name <> "description" .= desc <> "usage" .= usage <> "options" .= opts <> "subcommands" .= subcommands)
  toEncoding (Command name desc usage opts [] version) =
    pairs ("name" .= name <> "description" .= desc <> "usage" .= usage <> "options" .= opts <> "version" .= version)
  toEncoding (Command name desc usage opts subcommands version) =
    pairs ("name" .= name <> "description" .= desc <> "usage" .= usage <> "options" .= opts <> "subcommands" .= subcommands <> "version" .= version)

instance ToJSON Subcommand where
  toJSON (Subcommand name desc) =
    object ["name" .= name, "desc" .= desc]

  toEncoding (Subcommand name desc) =
    pairs ("name" .= name <> "desc" .= desc)

instance FromJSON Subcommand where
  parseJSON = withObject "Subcommand" $ \v ->
    Subcommand
      <$> (v .: "name" <|> v .: "cmd")  -- Accept both for backward compatibility
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
asSubcommand (Command n desc _ _ _ _) = Subcommand n desc
