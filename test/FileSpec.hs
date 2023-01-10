module FileSpec (spec) where

import File
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..))
import Parse (microTimestampToUTC)
import Test.Hspec

spec :: Spec
spec = do
  describe "noteToFile" $ do
    it "generates the correct path for an untitled text note" $ do
      path (noteToFile basicNote) `shouldBe` "Language/I'm a note! :D.md"

    it "generates the correct path for a checklist" $ do
      path (noteToFile checklist) `shouldBe` "pinned/Trips.md"

    it "generates the correct path for an untitled checklist" $ do
      path (noteToFile untitledChecklist) `shouldBe` "Lists/vim, neovim, vs code, sublime, ed.md"

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

untitledChecklist :: Note
untitledChecklist =
  Note
    { metadata =
        Metadata
          { tags = ["Lists"],
            lastEditedTime = microTimestampToUTC 1638147241847000,
            createdTime = microTimestampToUTC 1638145929577000,
            isTrashed = False,
            isPinned = False,
            isArchived = False
          },
      title = Nothing,
      content =
        Checklist
          [ ChecklistItem "vim" True,
            ChecklistItem "neovim" True,
            ChecklistItem "vs code" True,
            ChecklistItem "sublime" False,
            ChecklistItem "ed" False
          ]
    }
