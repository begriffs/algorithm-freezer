module Inter.Quora.StringsSpec where

import Test.Hspec

import Inter.Quora.Strings

spec :: Spec
spec = do
  describe "First non-repeated character" $ do
    it "None in empty string" $
      firstUniqueChar "" `shouldBe` Nothing
    it "None for string of dups" $
      firstUniqueChar "abcabc" `shouldBe` Nothing
    it "One at end of string" $
      firstUniqueChar "aabbc" `shouldBe` Just 'c'
    it "Choses first of multiple possibilities" $
      firstUniqueChar "aabc" `shouldBe` Just 'b'
