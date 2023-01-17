module Note
  ( ChecklistItem (..),
    Metadata (..),
    Note (..),
    NoteContent (..),
    Tag,
    mkTag,
    mkTags,
    unTag,
  )
where

import Data.Char (isAlphaNum)
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

data NoteContent = Text T.Text | Checklist [ChecklistItem]
  deriving (Show, Eq)

data ChecklistItem = ChecklistItem
  { text :: T.Text,
    isChecked :: Bool
  }
  deriving (Show, Eq)

-- Create Tags using the `mkTag` constructor below.
newtype Tag = UnsafeTag T.Text
  deriving (Show, Eq)

-- 1. Replace a certain subset of characters
-- 2. Remove anything else that isn't "valid" (by Obsidian's tag rules)
-- 3. Construct the Tag
--
-- TODO: Obsidian doesn't like tags that are all numeric. Do something dumb
-- like stick a `-` at the end of such tags.
mkTag :: T.Text -> Tag
mkTag = UnsafeTag . removeInvalid . replaceInvalid

unTag :: Tag -> T.Text
unTag (UnsafeTag t) = t

mkTags :: [T.Text] -> [Tag]
mkTags = map mkTag

-- Replace a certain subset of characters with something else.
replaceInvalid :: T.Text -> T.Text
replaceInvalid = T.map repl
  where
    repl ' ' = '-'
    repl '/' = '-'
    repl '\\' = '-'
    repl other = other

-- After replacements are made, any other invalid characters will simply be
-- removed. Catering to Obsidian's requirements here, which are fairly strict.
-- See: https://help.obsidian.md/How+to/Working+with+tags#Allowed+characters
removeInvalid :: T.Text -> T.Text
removeInvalid = T.filter isValid
  where
    isValid '-' = True
    isValid '_' = True
    isValid other = isAlphaNum other
