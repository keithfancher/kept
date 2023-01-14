module Markdown
  ( Markdown,
    noteToMarkdown,
  )
where

import Data.Text qualified as T
import Data.Time (TimeZone, UTCTime, utcToZonedTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..), Tag)

type Markdown = T.Text

-- Convert a note to markdown. The time zone is used to display the timestamp
-- metadata in the user's time zone, rather than the default of UTC.
noteToMarkdown :: Note -> TimeZone -> Markdown
noteToMarkdown n tz =
  metadataToMarkdown (metadata n) tz
    <> "\n\n"
    <> titleToMarkdown (title n)
    <> contentToMarkdown (content n)

metadataToMarkdown :: Metadata -> TimeZone -> Markdown
metadataToMarkdown m tz =
  "---\n"
    <> "tags: "
    <> tagsToMarkdown (tags m)
    <> "\n"
    <> "createdTime: "
    <> timeToMarkdown (createdTime m) tz
    <> "\n"
    <> "lastEditedTime: "
    <> timeToMarkdown (lastEditedTime m) tz
    <> "\n"
    <> "---"

tagsToMarkdown :: [Tag] -> Markdown
tagsToMarkdown t = "[" <> commaSeparated <> "]"
  where
    commaSeparated = T.intercalate ", " t

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
