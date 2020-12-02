module AOCSpec where

import AOC 
import Test.Hspec

spec :: Spec
spec = 
    describe "day 1" $ do
       it "product all numbers combinations of 2 in expense file that sum to 2020" $ do
        result <- day1 
        result `shouldBe` [((954,1066),1016964)]
       it "product all numbers combinations of 3 in expense file that sum to 2020" $ do
        result <- day1Part2 
        result `shouldBe` [182588480]
       it "give me all valid password" $ do
        result <- day2 
        result `shouldBe` 550 
