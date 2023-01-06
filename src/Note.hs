module Note
  ( someFunc,
    Note (..),
  )
where

import Data.Text qualified as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Note = Note
  { metadata :: Metadata,
    content :: Content
  }

data Metadata = Metadata
  { createdTime :: Int, -- TODO: different type for time
    lastEditedTime :: Int,
    tags :: [Tag],
    isTrashed :: Bool,
    isPinned :: Bool,
    isArchived :: Bool
  }

type Tag = T.Text

data Content = Text T.Text | Checklist

type Checklist = [ChecklistItem]

data ChecklistItem = ChecklistItem
  { text :: T.Text,
    isChecked :: Bool
  }
