module Markdown
  ( Markdown,
    noteToMarkdown,
  )
where

import Data.Text qualified as T
import Note (ChecklistItem (..), Metadata, Note (..), NoteContent (..))

type Markdown = T.Text

noteToMarkdown :: Note -> Markdown
noteToMarkdown n =
  metadataToMarkdown (metadata n)
    <> "\n\n"
    <> contentToMarkdown (content n) -- TODO: title

-- TODO
metadataToMarkdown :: Metadata -> Markdown
metadataToMarkdown m =
  "---\n"
    <> "tags: []\n"
    <> "createdTime: 0\n"
    <> "lastEditedTime: 0\n"
    <> "---"

contentToMarkdown :: NoteContent -> Markdown
contentToMarkdown (Text t) = t
contentToMarkdown (Checklist l) = T.intercalate "\n" (map listItemToMarkdown l)

listItemToMarkdown :: ChecklistItem -> Markdown
listItemToMarkdown (ChecklistItem t True) = "- [x] " <> t
listItemToMarkdown (ChecklistItem t False) = "- [ ] " <> t
