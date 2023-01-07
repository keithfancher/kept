module Parse (parseNote) where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy as B
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Note (Metadata (..), Note (..), NoteContent (..))

type KeepJSON = T.Text

-- Given the JSON for a single Google Keep note, attempt to parse out our
-- internal Note type.
parseNote :: KeepJSON -> Maybe Note
parseNote json = mapNote <$> parseKeepJson json

-- TODO: docs
parseKeepJson :: KeepJSON -> Maybe KeepNote
parseKeepJson = decode . B.fromStrict . encodeUtf8

mapNote :: KeepNote -> Note
mapNote note@(KeepNote trash pinned archive noteTitle edited created _ _) =
  Note
    { metadata =
        Metadata
          { tags = [], -- TODO
            lastEditedTime = edited,
            createdTime = created,
            isArchived = archive,
            isPinned = pinned,
            isTrashed = trash
          },
      title = case noteTitle of
        "" -> Nothing
        nonEmpty -> Just nonEmpty,
      content = mapNoteContent note
    }

-- Note that note content must be text OR a checklist, but NOT both.
mapNoteContent :: KeepNote -> NoteContent
mapNoteContent n = case (textContent n, listContent n) of
  (Just textContent, Nothing) -> Text textContent
  (Nothing, Just listContent) -> Checklist [] -- TODO
  _ -> error "invalid case" -- TODO: Either

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
