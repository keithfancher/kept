module Note
  ( ChecklistItem (..),
    Metadata (..),
    Note (..),
    NoteContent (..),
    Tag,
  )
where

import Data.Text qualified as T
import Data.Time (UTCTime)

data Note = Note
  { metadata :: Metadata,
    title :: Maybe T.Text,
    content :: NoteContent
  }
  deriving (Show, Eq)

data Metadata = Metadata
  { createdTime :: UTCTime,
    lastEditedTime :: UTCTime,
    tags :: [Tag],
    isTrashed :: Bool,
    isPinned :: Bool,
    isArchived :: Bool
  }
  deriving (Show, Eq)

type Tag = T.Text

data NoteContent = Text T.Text | Checklist [ChecklistItem]
  deriving (Show, Eq)

data ChecklistItem = ChecklistItem
  { text :: T.Text,
    isChecked :: Bool
  }
  deriving (Show, Eq)
