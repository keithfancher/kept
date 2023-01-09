module Markdown
  ( Markdown,
    noteToMarkdown,
  )
where

import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..))

type Markdown = T.Text

noteToMarkdown :: Note -> Markdown
noteToMarkdown n =
  metadataToMarkdown (metadata n)
    <> "\n\n"
    <> titleToMarkdown (title n)
    <> contentToMarkdown (content n)

metadataToMarkdown :: Metadata -> Markdown
metadataToMarkdown m =
  "---\n"
    <> "tags: []\n" -- TODO: tags/labels
    <> "createdTime: "
    <> timeToMarkdown (createdTime m)
    <> "\n"
    <> "lastEditedTime: "
    <> timeToMarkdown (lastEditedTime m)
    <> "\n"
    <> "---"

titleToMarkdown :: Maybe T.Text -> Markdown
titleToMarkdown Nothing = ""
titleToMarkdown (Just t) = "# " <> t <> "\n\n"

contentToMarkdown :: NoteContent -> Markdown
contentToMarkdown (Text t) = t
contentToMarkdown (Checklist l) = T.intercalate "\n" (map listItemToMarkdown l)

listItemToMarkdown :: ChecklistItem -> Markdown
listItemToMarkdown (ChecklistItem t True) = "- [x] " <> t
listItemToMarkdown (ChecklistItem t False) = "- [ ] " <> t

-- TODO: use local time zone in output, probably? instead of UTC?
timeToMarkdown :: UTCTime -> Markdown
timeToMarkdown = T.pack . iso8601Show
