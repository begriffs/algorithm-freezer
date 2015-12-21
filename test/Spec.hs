import Test.Hspec

import qualified Data.Set as S
import Answers

main :: IO ()
main = hspec $
  describe "Most frequent int in array" $ do
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
