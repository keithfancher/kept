module Markdown
  ( Markdown,
    MarkdownOpts (..),
    TimeZones (..),
    noteToMarkdown,
    noteToMarkdownSystemTZ,
  )
where

import Data.List (intercalate)
import Data.Text qualified as T
import Data.Time (TimeZone, UTCTime, getTimeZone, utcToZonedTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Note (Attachment, ChecklistItem (..), Metadata (..), Note (..), NoteContent (..), Tag, getAttachments, unTag)

type Markdown = T.Text

data MarkdownOpts
  = FrontMatterDefault -- default fields included in YAML front-matter
  | FrontMatterWithTitle -- includes all default fields PLUS the note title, removes title heading in note body
  | NoFrontMatter -- no YAML front-matter at all

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
    <> titleToMarkdown mdOpts (title n)
    <> attachmentsToMarkdown (getAttachments n)
    <> contentToMarkdown (content n)
  where
    frontMatter = case mdOpts of
      NoFrontMatter -> ""
      _ -> yamlFrontMatter mdOpts n tz <> "\n\n"

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

yamlFrontMatter :: MarkdownOpts -> Note -> TimeZones -> Markdown
yamlFrontMatter opts (Note m t _) (TimeZones createdTz editedTz) =
  "---\n"
    <> titleYaml opts t
    <> "tags: "
    <> tagsYaml (tags m)
    <> "\n"
    <> "createdTime: "
    <> timeToMarkdown (createdTime m) createdTz
    <> "\n"
    <> "lastEditedTime: "
    <> timeToMarkdown (lastEditedTime m) editedTz
    <> "\n"
    <> attachmentPathsYaml (attachments m)
    <> "---"
  where
    -- the ONLY case where we output a title in the front-matter:
    titleYaml FrontMatterWithTitle (Just title) = "title: " <> title <> "\n"
    titleYaml _ _ = ""

tagsYaml :: [Tag] -> Markdown
tagsYaml t = "[" <> commaSeparated <> "]"
  where
    commaSeparated = T.intercalate ", " $ tagsAsText t
    tagsAsText = map unTag

-- Outputs the title as level-one heading in the body of the note. If the title
-- is empty OR if the user has elected to put the title in the YAML
-- front-matter, we output nothing here.
titleToMarkdown :: MarkdownOpts -> Maybe T.Text -> Markdown
titleToMarkdown FrontMatterWithTitle _ = ""
titleToMarkdown _ Nothing = ""
titleToMarkdown _ (Just t) = "# " <> t <> "\n\n"

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

-- We're including the key name (`attachments`) in the output here. UNLIKE with
-- tags, if there are no attachments we don't want to include the field in the
-- markdown output at all.
attachmentPathsYaml :: [Attachment] -> Markdown
attachmentPathsYaml [] = ""
attachmentPathsYaml attachments = "attachments: [" <> commaSep attachments <> "]\n"
  where
    commaSep a = T.pack $ intercalate ", " a

-- Similar to above: if we have no attachments, nothing should be rendered.
attachmentsToMarkdown :: [Attachment] -> Markdown
attachmentsToMarkdown [] = ""
attachmentsToMarkdown as = T.intercalate "\n" (map attachmentToMd as) <> "\n\n"
  where
    attachmentToMd a = "![](" <> T.pack a <> ")"
