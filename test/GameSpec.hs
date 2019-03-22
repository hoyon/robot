module GameSpec (spec) where

import Test.Hspec
import Game

grid = Grid 2 2
robot = Robot 0 0 North False

spec :: Spec
spec = 
  describe "play" $ do
    it "without death gives final position" $
      play grid robot [F, F] `shouldBe` Robot 0 2 North False

    it "with death gives last known position" $
      play grid robot [L, F] `shouldBe` Robot 0 0 West True

    it "with slower death gives last known position" $
      play grid robot [F, F, R, F, L, F, F, F] `shouldBe` Robot 1 2 North True
