module Note
  ( someFunc,
    ChecklistItem (..),
    Metadata (..),
    Note (..),
    NoteContent (..),
    Tag,
  )
where

import Data.Text qualified as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Note = Note
  { metadata :: Metadata,
    content :: NoteContent
  }
  deriving (Show, Eq)

data Metadata = Metadata
  { createdTime :: Int, -- TODO: different type for time
    lastEditedTime :: Int,
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
