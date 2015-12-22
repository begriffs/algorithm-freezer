import Test.Hspec

import qualified Data.Set as S
import qualified Data.HashSet as H
import qualified Data.Vector as V

import Answers
import Types

main :: IO ()
main = hspec $ do
  describe "Most frequent item in array" $ do
    it "There is none in empty array" $
      mostFreq ([]::[Int]) `shouldBe` S.empty
    it "Chooses value from length 1 array" $
      mostFreq [0] `shouldBe` S.singleton 0
    it "Picks a simple winner" $
      mostFreq [0,0,1] `shouldBe` S.singleton 0
    it "Picks a simple winner in different order" $
      mostFreq [0,1,1] `shouldBe` S.singleton 1
    it "Detects a tie" $
      mostFreq [0,1] `shouldBe` S.fromList [0,1]

  describe "Pairs adding to ten" $ do
    let tenners = adders 10
    it "Lists no pairs for an empty list" $
      tenners [] `shouldBe` H.empty
    it "Lists no pairs when there are none" $
      tenners [0,1] `shouldBe` H.empty
    it "Finds a pair" $
      tenners [1,9] `shouldBe` H.singleton (makeUnordered (1, 9))
    it "Finds all pairs" $
      tenners [0,1,2,3,4,5,6,7,8,9] `shouldBe` H.fromList
        (map makeUnordered [(1,9), (2,8), (3,7), (4,6), (5,5)])

  describe "Detecting when one list is a rotation of another" $ do
    it "false for unequal lengths" $
      cycleEq [0,1] [] `shouldBe` False
    it "true for a typical example" $
      cycleEq [1,2,3,5,6,7,8] [5,6,7,8,1,2,3] `shouldBe` True
    it "true for lists with repeating elements" $
      cycleEq [1,1,1,1,2] [1,1,1,2,1] `shouldBe` True
    it "false for a strict sublist" $
      cycleEq [1] [1,1] `shouldBe` False

  describe "Items which occur only once in array" $ do
    it "Finds none in empty list" $
      loners ([]::[Int]) `shouldBe` S.empty
    it "Finds none when all are duped" $
      loners [0,0] `shouldBe` S.empty
    it "Identifies a single loner" $
      loners [0,1,1] `shouldBe` S.singleton 0
    it "Identifies two loners" $
      loners [0,1,1,2] `shouldBe` S.fromList [0,2]

  describe "Finding elements in common" $ do
    it "There are none for disjoint lists" $
      commonElts [0] [1] `shouldBe` S.empty
    it "Works as advertised" $
      commonElts [0,1,2] [1,2,3] `shouldBe` S.fromList [1,2]

  describe "Binary search on sorted array" $ do
    it "Finds nothing in an empty list" $
      sortedSearch 0 V.empty `shouldBe` Nothing
    it "Finds a value in a singleton list" $
      sortedSearch 0 (V.singleton 0) `shouldBe` Just 0
    it "Finds nothing in a bad singleton list" $
      sortedSearch 0 (V.singleton 1) `shouldBe` Nothing
    it "Finds a value on the extreme left" $
      sortedSearch 0 (V.fromList [0,1]) `shouldBe` Just 0
    it "Finds a value on the extreme right" $
      sortedSearch 1 (V.fromList [0,1]) `shouldBe` Just 1
    it "Finds middle index in a repeated list" $
      sortedSearch 0 (V.fromList [0,0,0]) `shouldBe` Just 1
    it "Fails to find value larger than all entries" $
      sortedSearch 2 (V.fromList [0,1]) `shouldBe` Nothing
    it "Fails to find value smaller than all entries" $
      sortedSearch 0 (V.fromList [1,2]) `shouldBe` Nothing
