module MarkdownSpec (spec) where

import Markdown
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "noteToMarkdown" $ do
    it "converts a text note" $ do
      noteToMarkdown basicNote `shouldBe` ""

    it "converts a checklist" $ do
      noteToMarkdown checklist `shouldBe` ""

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
