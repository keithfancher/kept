module Markdown
  ( Markdown,
    MarkdownOpts (..),
    TimeZones (..),
    noteToMarkdown,
    noteToMarkdownSystemTZ,
  )
where

import Data.Text qualified as T
import Data.Time (TimeZone, UTCTime, getTimeZone, utcToZonedTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..), Tag, unTag)

type Markdown = T.Text

data MarkdownOpts
  = YamlFrontMatter
  | NoFrontMatter

-- Wrapper for our the *two* time zones we need in order to properly localize
-- these two timestamps. Annoying! See the docs for `noteToMarkdownSystemTZ`
-- below for some more context.
data TimeZones = TimeZones
  { createdTz :: TimeZone,
    editedTz :: TimeZone
  }

-- Convert a note to markdown. The time zones are used to display the timestamp
-- metadata in the user's time zone, rather than the default of UTC.
noteToMarkdown :: MarkdownOpts -> Note -> TimeZones -> Markdown
noteToMarkdown mdOpts n tz =
  frontMatter
    <> titleToMarkdown (title n)
    <> contentToMarkdown (content n)
  where
    frontMatter = case mdOpts of
      YamlFrontMatter -> metadataToMarkdown (metadata n) tz <> "\n\n"
      NoFrontMatter -> ""

-- Addresses a subtle bug with daylight savings, aka "summer time". We can't
-- just get the system time zone, we have to get a *different* system time zone
-- for each distinct DateTime. In other words, what would our *current* time
-- zone have been at *that* point in time?
noteToMarkdownSystemTZ :: MarkdownOpts -> Note -> IO Markdown
noteToMarkdownSystemTZ mdOpts n = do
  createdTz <- getTimeZone $ created n
  editedTz <- getTimeZone $ edited n
  let tz = TimeZones {createdTz = createdTz, editedTz = editedTz}
  return $ noteToMarkdown mdOpts n tz
  where
    created = createdTime . metadata
    edited = lastEditedTime . metadata

metadataToMarkdown :: Metadata -> TimeZones -> Markdown
metadataToMarkdown m (TimeZones createdTz editedTz) =
  "---\n"
    <> "tags: "
    <> tagsToMarkdown (tags m)
    <> "\n"
    <> "createdTime: "
    <> timeToMarkdown (createdTime m) createdTz
    <> "\n"
    <> "lastEditedTime: "
    <> timeToMarkdown (lastEditedTime m) editedTz
    <> "\n"
    <> "---"

tagsToMarkdown :: [Tag] -> Markdown
tagsToMarkdown t = "[" <> commaSeparated <> "]"
  where
    commaSeparated = T.intercalate ", " $ tagsAsText t
    tagsAsText = map unTag

titleToMarkdown :: Maybe T.Text -> Markdown
titleToMarkdown Nothing = ""
titleToMarkdown (Just t) = "# " <> t <> "\n\n"

contentToMarkdown :: NoteContent -> Markdown
contentToMarkdown (Text t) = t
contentToMarkdown (Checklist l) = T.intercalate "\n" (map listItemToMarkdown l)

listItemToMarkdown :: ChecklistItem -> Markdown
listItemToMarkdown (ChecklistItem t True) = "- [x] " <> t
listItemToMarkdown (ChecklistItem t False) = "- [ ] " <> t

timeToMarkdown :: UTCTime -> TimeZone -> Markdown
timeToMarkdown utcTime tz = T.pack $ iso8601Show zoned
  where
    zoned = utcToZonedTime tz utcTime
