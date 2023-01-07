module MarkdownSpec (spec) where

import Markdown
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..))
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
            lastEditedTime = 1632886343121000,
            createdTime = 1632886283906000,
            isTrashed = False,
            isPinned = False,
            isArchived = False
          },
      title = Nothing,
      content = Text "I'm a note! :D"
    }

basicNoteMarkdown :: Markdown
basicNoteMarkdown = "---\ntags: []\ncreatedTime: 0\nlastEditedTime: 0\n---\n\nI'm a note! :D"

checklist :: Note
checklist =
  Note
    { metadata =
        Metadata
          { tags = ["Goals", "Travel"],
            lastEditedTime = 1638147241847000,
            createdTime = 1638145929577000,
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
checklistMarkdown = "---\ntags: []\ncreatedTime: 0\nlastEditedTime: 0\n---\n\n- [x] France\n- [x] Japan\n- [x] Italy\n- [ ] Spain\n- [ ] Iceland"
