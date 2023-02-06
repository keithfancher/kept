module ParseSpec (spec) where

import Data.Either (isLeft)
import Data.Text qualified as T
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..), mkTags)
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parseNote" $ do
    it "fails if given an empty string input" $ do
      parseNote "" `shouldSatisfy` isLeft

    it "parses a basic text note" $ do
      parseNote basicNoteJson `shouldBe` Right basicNoteOutput

    it "parses a checklist" $ do
      parseNote checklistJson `shouldBe` Right checklistOutput

    it "parses a note with attachments" $ do
      parseNote attachmentJson `shouldBe` Right attachmentOutput

basicNoteJson :: T.Text
basicNoteJson = "{\"color\":\"DEFAULT\",\"isTrashed\":false,\"isPinned\":false,\"isArchived\":false,\"textContent\":\"I'm a note! :D\",\"title\":\"\",\"userEditedTimestampUsec\":1632886343121000,\"createdTimestampUsec\":1632886283906000,\"labels\":[{\"name\":\"Language\"}]}"

basicNoteOutput :: Note
basicNoteOutput =
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

checklistJson :: T.Text
checklistJson = "{\"color\":\"DEFAULT\",\"isTrashed\":false,\"isPinned\":true,\"isArchived\":false,\"annotations\":[],\"listContent\":[{\"text\":\"France\",\"isChecked\":true},{\"text\":\"Japan\",\"isChecked\":true},{\"text\":\"Italy\",\"isChecked\":true},{\"text\":\"Spain\",\"isChecked\":false},{\"text\":\"Iceland\",\"isChecked\":false}],\"title\":\"Trips\",\"userEditedTimestampUsec\":1638147241847000,\"createdTimestampUsec\":1638145929577000,\"labels\":[{\"name\":\"Goals\"},{\"name\":\"Travel\"}]}"

checklistOutput :: Note
checklistOutput =
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

attachmentJson :: T.Text
attachmentJson = "{\"attachments\":[{\"filePath\":\"1748a3164a3.8ab3a6025474d7d8.jpg\",\"mimetype\":\"image/jpeg\"}],\"color\":\"DEFAULT\",\"isTrashed\":false,\"isPinned\":false,\"isArchived\":false,\"textContent\":\"Rockin' image attached\",\"title\":\"\",\"userEditedTimestampUsec\":1632886343121000,\"createdTimestampUsec\":1632886283906000,\"labels\":[{\"name\":\"cat stuff\"}]}"

attachmentOutput :: Note
attachmentOutput =
  Note
    { metadata =
        Metadata
          { tags = mkTags ["cat-stuff"],
            lastEditedTime = microTimestampToUTC 1632886343121000,
            createdTime = microTimestampToUTC 1632886283906000,
            attachments = ["1748a3164a3.8ab3a6025474d7d8.jpg"],
            isTrashed = False,
            isPinned = False,
            isArchived = False
          },
      title = Nothing,
      content = Text "Rockin' image attached"
    }
