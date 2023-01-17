module NoteSpec (spec) where

import Data.Text qualified as T
import Note
import Test.Hspec

spec :: Spec
spec = do
  describe "mkTag" $ do
    it "leaves valid tag text unchanged" $ do
      mkTagText "cat-vids02" `shouldBe` "cat-vids02"

    it "replaces slashes and spaces" $ do
      mkTagText "stuff/things and whatnot" `shouldBe` "stuff-things-and-whatnot"

    it "removes invalid characters" $ do
      mkTagText "All the Stuff!!!?!" `shouldBe` "All-the-Stuff"

    it "creates valid tags with unicode characters" $ do
      mkTagText "大事な事!!!" `shouldBe` "大事な事"

-- Helper to make testing these outputs easier. Transform the Text to a Tag,
-- then pull Text back out.
mkTagText :: T.Text -> T.Text
mkTagText = unTag . mkTag
