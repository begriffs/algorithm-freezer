module Inter.Quora.StringsSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V

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

  describe "Reversing a vector" $ do
    it "doing so with recursion behaves the same as std lib" $ property $ \s ->
      let v = V.fromList (s::String) in
      revRecursive (v::V.Vector Char) `shouldBe` V.reverse v
    it "doing so with mutation behaves the same as std lib" $ property $ \s ->
      let v = V.fromList (s::String) in
      revIterative (v::V.Vector Char) `shouldBe` V.reverse v
