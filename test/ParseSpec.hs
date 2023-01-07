module ParseSpec (spec) where

import Data.Text qualified as T
import Note (ChecklistItem (..), Metadata (..), Note (..), NoteContent (..))
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parseNote" $ do
    it "returns Nothing if given an empty string input" $ do
      parseNote "" `shouldBe` Nothing

    it "parses a basic text note" $ do
      parseNote basicNoteJson `shouldBe` Just basicNoteOutput

    it "parses a checklist" $ do
      parseNote checklistJson `shouldBe` Just checklistOutput

basicNoteJson :: T.Text
basicNoteJson = "{\"color\":\"DEFAULT\",\"isTrashed\":false,\"isPinned\":false,\"isArchived\":false,\"textContent\":\"I'm a note! :D\",\"title\":\"\",\"userEditedTimestampUsec\":1632886343121000,\"createdTimestampUsec\":1632886283906000,\"labels\":[{\"name\":\"Language\"}]}"

basicNoteOutput :: Note
basicNoteOutput =
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

checklistJson :: T.Text
checklistJson = "{\"color\":\"DEFAULT\",\"isTrashed\":false,\"isPinned\":true,\"isArchived\":false,\"annotations\":[],\"listContent\":[{\"text\":\"France\",\"isChecked\":true},{\"text\":\"Japan\",\"isChecked\":true},{\"text\":\"Italy\",\"isChecked\":true},{\"text\":\"Spain\",\"isChecked\":false},{\"text\":\"Iceland\",\"isChecked\":false}],\"title\":\"Trips\",\"userEditedTimestampUsec\":1638147241847000,\"createdTimestampUsec\":1638145929577000,\"labels\":[{\"name\":\"Goals\"},{\"name\":\"Travel\"}]}"

checklistOutput :: Note
checklistOutput =
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
