import Test.Hspec

import qualified Data.Set as S
import qualified Data.HashSet as H

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
