module MarkdownSpec (spec) where

import Markdown
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..))
import Parse (microTimestampToUTC)
import Test.Hspec

spec :: Spec
spec = do
  describe "noteToMarkdown" $ do
    it "converts a text note" $ do
      noteToMarkdown basicNote `shouldBe` basicNoteMarkdown

    it "converts a checklist" $ do
      noteToMarkdown checklist `shouldBe` checklistMarkdown

basicNote :: Note
basicNote =
  Note
    { metadata =
        Metadata
          { tags = ["Language"],
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
basicNoteMarkdown = "---\ntags: []\ncreatedTime: 2021-09-29T03:31:23Z\nlastEditedTime: 2021-09-29T03:32:23Z\n---\n\nI'm a note! :D"

checklist :: Note
checklist =
  Note
    { metadata =
        Metadata
          { tags = ["Goals", "Travel"],
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
checklistMarkdown = "---\ntags: []\ncreatedTime: 2021-11-29T00:32:09Z\nlastEditedTime: 2021-11-29T00:54:01Z\n---\n\n- [x] France\n- [x] Japan\n- [x] Italy\n- [ ] Spain\n- [ ] Iceland"
