module ParseSpec (spec) where

import Data.Text qualified as T
import Note (Metadata (..), Note (..), NoteContent (..))
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parseNote" $ do
    it "returns Nothing if given an empty string input" $ do
      parseNote "" `shouldBe` Nothing

  describe "parseNote" $ do
    it "parses a basic text note" $ do
      parseNote basicNoteJson `shouldBe` Just basicNoteOutput

basicNoteJson :: T.Text
basicNoteJson = "{\"color\":\"DEFAULT\",\"isTrashed\":false,\"isPinned\":false,\"isArchived\":false,\"textContent\":\"I'm a note! :D\",\"title\":\"\",\"userEditedTimestampUsec\":1632886343121000,\"createdTimestampUsec\":1632886283906000,\"labels\":[{\"name\":\"Language\"}]}"

basicNoteOutput :: Note
basicNoteOutput =
  Note
    { metadata =
        Metadata
          { tags = [], -- TODO
            lastEditedTime = 0, -- TODO
            createdTime = 0, -- TODO
            isTrashed = False,
            isPinned = False,
            isArchived = False
          },
      content = Text "I'm a note! :D"
    }
