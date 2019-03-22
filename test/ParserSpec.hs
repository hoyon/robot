module ParserSpec (spec) where

import Text.Parsec
import Test.Hspec
import Parser
import Game

spec :: Spec
spec =
    describe "Parser" $ do
      describe "grid" $ do
        it "correctly parses grid" $
          parse grid "" "1 1" `shouldBe` Right (Grid 1 1)
        it "rejects bad input" $
          parse grid "" "not a grid" `shouldSatisfy` isLeft

      describe "robot" $ do
        it "correctly parses robot" $
          parse robot "" "(0, 1, N) FRL" `shouldBe` Right ((Robot 0 1 North False), [F, R, L])
        it "rejects bad input" $
          parse robot "" "not a robot" `shouldSatisfy` isLeft


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
