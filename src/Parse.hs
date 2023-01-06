module Parse (parseNote) where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy as B
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Note (Note)

-- TODO!
parseNote :: T.Text -> Maybe Note
parseNote = undefined

-- TODO: docs
parseKeepJson :: T.Text -> Maybe KeepNote
parseKeepJson = decode . B.fromStrict . encodeUtf8

-- An intermediary data type that mirrors the JSON structure for easy parsin'.
data KeepNote = KeepNote
  { isTrashed :: Bool,
    isPinned :: Bool,
    isArchived :: Bool,
    title :: T.Text,
    userEditedTimestampUsec :: Int,
    createdTimestampUsec :: Int,
    textContent :: Maybe T.Text,
    listContent :: Maybe [KeepListItem]
    -- TODO: labels
  }
  deriving (Generic, Show)

instance FromJSON KeepNote

data KeepListItem = KeepListItem
  { text :: T.Text,
    isChecked :: Bool
  }
  deriving (Generic, Show)

instance FromJSON KeepListItem
