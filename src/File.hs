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
    subdir = noteSubDir (metadata n)

-- Get the filename for a note. NOT the full path. See also `noteSubDir`, below.
noteFilename :: Note -> FilePath
noteFilename (Note _ title cont) = makeValidT $ removeDelimiters $ base <> ext
  where
    base = T.strip $ case title of -- Note `strip`: prevents awkward whitespace b/w base and extension
      Nothing -> titleFromContent cont
      Just t -> t
    makeValidT = makeValid . T.unpack -- valid FilePath from Text
    ext = ".md"

-- Technically a filename is "valid" even if it contains path delimiters (at
-- least according to `System.FilePath`). That doesn't work for our case
-- though, e.g. if the title of a note is `Stuff/Things`, we don't want it to
-- create a `Stuff` directory. Also apparently newlines can exist in a
-- filename?!
removeDelimiters :: T.Text -> T.Text
removeDelimiters = T.map replaceSlashes
  where
    replaceSlashes '/' = '-'
    replaceSlashes '\\' = '-'
    replaceSlashes '\n' = ' '
    replaceSlashes '\r' = ' '
    replaceSlashes nonSlash = nonSlash

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
noteSubDir :: Metadata -> FilePath
noteSubDir (Metadata _ _ _ True _ _) = "trash"
noteSubDir (Metadata _ _ _ _ _ True) = "archive"
noteSubDir (Metadata _ _ _ _ True _) = "pinned"
noteSubDir (Metadata _ _ [] _ _ _) = "untagged"
noteSubDir (Metadata _ _ tags _ _ _) = T.unpack $ T.intercalate "-" tags -- TODO: sort tags (case sens?)
