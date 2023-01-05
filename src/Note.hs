module Note
  ( someFunc,
    Note (..),
  )
where

import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Note = Note
  { metadata :: MetaData,
    content :: Content
  }

data MetaData = MetaData
  { createdTime :: Int, -- TODO: different type for time
    lastEditedTime :: Int,
    tags :: [Tag]
  }

type Tag = T.Text

data Content = Text T.Text | Checklist

type Checklist = [ChecklistItem]

data ChecklistItem = ChecklistItem
  { text :: T.Text,
    isChecked :: Bool
  }
