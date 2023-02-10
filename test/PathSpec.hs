module PathSpec (spec) where

import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..), mkTags)
import Parse (microTimestampToUTC)
import Path
import Test.Hspec

spec :: Spec
spec = do
  describe "getNotePath" $ do
    it "generates the correct path for an untitled text note" $ do
      getNotePath basicNote TagSubDirs `shouldBe` "Language/I'm a note! :D.md"

    it "generates the correct path when note has a path delimiter in the title" $ do
      getNotePath checklist TagSubDirs `shouldBe` "pinned/Trips-To Live.md"

    it "generates the correct path for an untitled checklist" $ do
      getNotePath untitledChecklist TagSubDirs `shouldBe` "Lists/vim, neovim, vs code, sublime, ed.md"

    it "generates the correct path for note with multiple tags" $ do
      getNotePath multiTagNote TagSubDirs `shouldBe` "Important-Tasks/Memo to myself.md"

    it "generates a path without tag-based subdirectories" $ do
      getNotePath multiTagNote NoTagSubDirs `shouldBe` "all-notes/Memo to myself.md"

basicNote :: Note
basicNote =
  Note
    { metadata =
        Metadata
          { tags = mkTags ["Language"],
            lastEditedTime = microTimestampToUTC 1632886343121000,
            createdTime = microTimestampToUTC 1632886283906000,
            attachments = [],
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
          { tags = mkTags ["Goals", "Travel"],
            lastEditedTime = microTimestampToUTC 1638147241847000,
            createdTime = microTimestampToUTC 1638145929577000,
            attachments = [],
            isTrashed = False,
            isPinned = True,
            isArchived = False
          },
      title = Just "Trips/To Live", -- Note: This won't work as-is as a filename D:
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
          { tags = mkTags ["Lists"],
            lastEditedTime = microTimestampToUTC 1638147241847000,
            createdTime = microTimestampToUTC 1638145929577000,
            attachments = [],
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

multiTagNote :: Note
multiTagNote =
  Note
    { metadata =
        Metadata
          { tags = mkTags ["Tasks", "Important"],
            lastEditedTime = microTimestampToUTC 1638147241847000,
            createdTime = microTimestampToUTC 1638145929577000,
            attachments = [],
            isTrashed = False,
            isPinned = False,
            isArchived = False
          },
      title = Just "Memo to myself",
      content = Text "Do the dumb things I gotta do"
    }
