module Path
  ( PathOptions (..),
    getNotePath,
  )
where

import Data.List (sort)
import Data.Text qualified as T
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..), Tag, unTag)
import System.FilePath (makeValid, (</>))

data PathOptions
  = TagSubDirs -- Create a subdirectory for the note based on its tag(s)
  | NoTagSubDirs -- Dump all notes into the same directory structure regardless of tag

-- The logic to determine the file path for a given note.
getNotePath :: Note -> PathOptions -> FilePath
getNotePath n pathOpts = makeValid filenameWithPath
  where
    filenameWithPath = subdir </> noteFilename n
    subdir = noteSubDir (metadata n) pathOpts

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

-- If a note doesn't have a title, generate one by grabbing the first
-- `titleLength` characters from the content. If it's a checklist, first
-- combine the list items.
titleFromContent :: NoteContent -> T.Text
titleFromContent (Text t) = T.take titleLength t
titleFromContent (Checklist c) = T.take titleLength $ listAsText c
  where
    listAsText l = T.intercalate ", " (map text l)

-- How many characters of content we'll grab to generate a title. Somewhat
-- arbitrarily chose to be "long enough". But not "too long". Whatever that
-- means!
titleLength :: Int
titleLength = 45

-- Sort the notes into subdirectories. Prioritize trash, archive, and pinned.
-- (In that order. For example, if a note is trashed, we don't care about its
-- tags, or that it's pinned.)
noteSubDir :: Metadata -> PathOptions -> FilePath
noteSubDir (Metadata _ _ _ True _ _) _ = "trash"
noteSubDir (Metadata _ _ _ _ _ True) _ = "archive"
noteSubDir (Metadata _ _ _ _ True _) _ = "pinned"
-- If not trashed/archive/pinned, check the tag-based path options:
noteSubDir _ NoTagSubDirs = "all-notes"
noteSubDir m TagSubDirs = subDirFromTags $ tags m

-- Use a sorted combo of tags if the user has opted for tag-based
-- subdirectories. Works best if notes only had a single label in Keep,
-- obviously.
subDirFromTags :: [Tag] -> FilePath
subDirFromTags [] = "untagged"
subDirFromTags tags = T.unpack $ T.intercalate "-" $ sortTags tags
  where
    sortTags = sort . map unTag
