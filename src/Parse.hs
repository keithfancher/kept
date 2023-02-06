module Parse
  ( KeepJSON,
    ParseError,
    parseNote,
    microTimestampToUTC,
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as B
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Note (Attachment, ChecklistItem (..), Metadata (..), Note (..), NoteContent (..), Tag, mkTag)

type ParseError = String

type KeepJSON = T.Text

-- Given the JSON for a single Google Keep note, attempt to parse out our
-- internal Note type.
parseNote :: KeepJSON -> Either ParseError Note
parseNote json = parseKeepJson json >>= mapNote

-- First encode (utf8) Text to a ByteString. But Aeson requires a *lazy*
-- ByteString, so we have to convert one more time.
parseKeepJson :: KeepJSON -> Either ParseError KeepNote
parseKeepJson = eitherDecode . B.fromStrict . encodeUtf8

mapNote :: KeepNote -> Either ParseError Note
mapNote note@(KeepNote trash pinned archive noteTitle edited created labels attachments _ _) = do
  noteContent <- mapNoteContent note
  return
    Note
      { metadata =
          Metadata
            { tags = mapLabels labels,
              lastEditedTime = microTimestampToUTC edited,
              createdTime = microTimestampToUTC created,
              isArchived = archive,
              isPinned = pinned,
              isTrashed = trash,
              attachments = mapAttachments attachments
            },
        title = case noteTitle of
          "" -> Nothing
          nonEmpty -> Just nonEmpty,
        content = noteContent
      }

mapNoteContent :: KeepNote -> Either ParseError NoteContent
mapNoteContent n = case (textContent n, listContent n) of
  (Just textContent, Nothing) -> Right $ Text textContent
  (Nothing, Just listContent) -> Right $ Checklist $ map mapChecklistItem listContent
  -- Any other case is technically not even possible? In theory...
  _ -> Left "Invalid JSON input: must contain a list or text, but not both"

mapChecklistItem :: KeepListItem -> ChecklistItem
mapChecklistItem (KeepListItem t c) = ChecklistItem t c

mapLabels :: Maybe [KeepLabel] -> [Tag]
mapLabels Nothing = []
mapLabels (Just labels) = map tagify labels
  where
    tagify = mkTag . name

mapAttachments :: Maybe [KeepAttachment] -> [Attachment]
mapAttachments Nothing = []
mapAttachments (Just a) = map fromKeep a
  where
    fromKeep = T.unpack . filePath

-- Input data has a microsecond timestamp, convert to `UTCTime`.
microTimestampToUTC :: Int -> UTCTime
microTimestampToUTC microTs = posixSecondsToUTCTime $ fromIntegral tsSeconds
  where
    tsSeconds = microTs `div` 1000000

-- An intermediary data type that mirrors the JSON structure for easy parsin'.
data KeepNote = KeepNote
  { isTrashed :: Bool,
    isPinned :: Bool,
    isArchived :: Bool,
    title :: T.Text,
    userEditedTimestampUsec :: Int,
    createdTimestampUsec :: Int,
    labels :: Maybe [KeepLabel],
    attachments :: Maybe [KeepAttachment],
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

-- The JSON also contains a `mimetype` field that we don't care about.
data KeepAttachment = KeepAttachment
  { filePath :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON KeepAttachment
