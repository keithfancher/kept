module File
  ( File (..),
    noteToFile,
  )
where

import Data.Text qualified as T
import Markdown (noteToMarkdown)
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..))
import System.FilePath (makeValid, (</>))

-- Simple container for text that lives at some filepath.
data File = File
  { path :: FilePath,
    content :: T.Text
  }
  deriving (Show, Eq)

noteToFile :: Note -> File
noteToFile n =
  File
    { path = makeValid filenameWithPath,
      content = noteToMarkdown n
    }
  where
    filenameWithPath = subdir </> noteFilename n
    subdir = notePath (metadata n)

-- Get the filename for a note. NOT the full path. See also: `notePath`, below.
--
-- TODO: will makeValid remove `/`? Or just think it's part of the path?
-- Probably the latter, might need a bit of my own filtering too.
noteFilename :: Note -> FilePath
noteFilename (Note _ title cont) = makeValidT $ base <> ext
  where
    base = case title of
      Nothing -> titleFromContent cont
      Just t -> t
    makeValidT = makeValid . T.unpack -- valid FilePath from Text
    ext = ".md"

-- If a note doesn't have a title, generate one by grabbing the first 35
-- characters from the content. (35 is arbitrary -- long enough, not too long.
-- Whatever that means.) If it's a checklist, first combine the list items.
--
-- TODO: Include (last edited) date in this? Need a decent chance of uniqueness...
titleFromContent :: NoteContent -> T.Text
titleFromContent (Text t) = T.take 35 t
titleFromContent (Checklist c) = T.take 35 $ listAsText c
  where
    listAsText l = T.intercalate ", " (map text l)

-- Sort the notes into subdirectories. Prioritize trash, archive, and pinned.
-- (In that order. For example, if a note is trashed, we don't care about its
-- tags, or that it's pinned.) After that, use a sorted combo of tags for
-- subdirs. Works best if notes only had a single label in Keep, obviously.
--
-- TODO: could split by year? add option for NO subdirs, depending on your
-- software that might be easiest. (Just use the tag metadata to sort through
-- your notes.)
notePath :: Metadata -> FilePath
notePath (Metadata _ _ _ True _ _) = "trash"
notePath (Metadata _ _ _ _ _ True) = "archive"
notePath (Metadata _ _ _ _ True _) = "pinned"
notePath (Metadata _ _ [] _ _ _) = "untagged"
notePath (Metadata _ _ tags _ _ _) = T.unpack $ T.intercalate "-" tags -- TODO: sort tags (case sens?)
