module MarkdownSpec (spec) where

import Data.Time (TimeZone (..))
import Markdown
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..), mkTags)
import Parse (microTimestampToUTC)
import Test.Hspec

spec :: Spec
spec = do
  describe "noteToMarkdown" $ do
    it "converts a text note" $ do
      noteToMarkdown basicNote pst `shouldBe` basicNoteMarkdown

    it "converts a checklist" $ do
      noteToMarkdown checklist pst `shouldBe` checklistMarkdown

pst :: TimeZone
pst = TimeZone {timeZoneSummerOnly = False, timeZoneMinutes = -480, timeZoneName = "PST"}

basicNote :: Note
basicNote =
  Note
    { metadata =
        Metadata
          { tags = mkTags ["Language"],
            lastEditedTime = microTimestampToUTC 1632886343121000,
            createdTime = microTimestampToUTC 1632886283906000,
            isTrashed = False,
            isPinned = False,
            isArchived = False
          },
      title = Nothing,
      content = Text "I'm a note! :D"
    }

basicNoteMarkdown :: Markdown
basicNoteMarkdown = "---\ntags: [Language]\ncreatedTime: 2021-09-28T19:31:23-08:00\nlastEditedTime: 2021-09-28T19:32:23-08:00\n---\n\nI'm a note! :D"

checklist :: Note
checklist =
  Note
    { metadata =
        Metadata
          { tags = mkTags ["Goals", "Travel"],
            lastEditedTime = microTimestampToUTC 1638147241847000,
            createdTime = microTimestampToUTC 1638145929577000,
            isTrashed = False,
            isPinned = True,
            isArchived = False
          },
      title = Just "Trips",
      content =
        Checklist
          [ ChecklistItem "France" True,
            ChecklistItem "Japan" True,
            ChecklistItem "Italy" True,
            ChecklistItem "Spain" False,
            ChecklistItem "Iceland" False
          ]
    }

checklistMarkdown :: Markdown
checklistMarkdown = "---\ntags: [Goals, Travel]\ncreatedTime: 2021-11-28T16:32:09-08:00\nlastEditedTime: 2021-11-28T16:54:01-08:00\n---\n\n# Trips\n\n- [x] France\n- [x] Japan\n- [x] Italy\n- [ ] Spain\n- [ ] Iceland"
