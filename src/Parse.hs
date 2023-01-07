module Parse (parseNote) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as B
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..), Tag)

type Error = String -- TODO: real error type

type KeepJSON = T.Text

-- Given the JSON for a single Google Keep note, attempt to parse out our
-- internal Note type.
parseNote :: KeepJSON -> Either Error Note
parseNote json = mapNote <$> parseKeepJson json

-- First encode (utf8) Text to a ByteString. But Aeson requires a *lazy*
-- ByteString, so we have to convert one more time.
parseKeepJson :: KeepJSON -> Either Error KeepNote
parseKeepJson = eitherDecode . B.fromStrict . encodeUtf8

mapNote :: KeepNote -> Note
mapNote note@(KeepNote trash pinned archive noteTitle edited created labels _ _) =
  Note
    { metadata =
        Metadata
          { tags = mapLabels labels,
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
  (Nothing, Just listContent) -> Checklist $ map mapChecklistItem listContent
  _ -> error "invalid case" -- TODO: Use eithers, return a left

mapChecklistItem :: KeepListItem -> ChecklistItem
mapChecklistItem (KeepListItem t c) = ChecklistItem t c

mapLabels :: Maybe [KeepLabel] -> [Tag]
mapLabels Nothing = []
mapLabels (Just labels) = map name labels

-- An intermediary data type that mirrors the JSON structure for easy parsin'.
data KeepNote = KeepNote
  { isTrashed :: Bool,
    isPinned :: Bool,
    isArchived :: Bool,
    title :: T.Text,
    userEditedTimestampUsec :: Int,
    createdTimestampUsec :: Int,
    labels :: Maybe [KeepLabel],
    textContent :: Maybe T.Text,
    listContent :: Maybe [KeepListItem]
  }
  deriving (Generic, Show)

instance FromJSON KeepNote

data KeepListItem = KeepListItem
  { text :: T.Text,
    isChecked :: Bool
  }
  deriving (Generic, Show)

instance FromJSON KeepListItem

-- Labels == tags
data KeepLabel = KeepLabel
  { name :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON KeepLabel
