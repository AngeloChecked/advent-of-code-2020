module AOCSpec where

import AOC 
import Test.Hspec

spec :: Spec
spec = 
    describe "all AOC results" $ do
       it "product all numbers combinations of 2 in expense file that sum to 2020" $ do
        result <- day1 
        result `shouldBe` [((954,1066),1016964)]
       it "product all numbers combinations of 3 in expense file that sum to 2020" $ do
        result <- day1Part2 
        result `shouldBe` [182588480]
       it "give me all valid password" $ do
        result <- day2 
        result `shouldBe` 550 
       it "give me all valid password (part2)" $ do
        result <- day2Part2 
        result `shouldBe` 634 
       it "give me all encountered threes in the map" $ do
        result <- day3 
        result `shouldBe` 205 
       it "product all encountered threes moving in different patterns in the map (part2)" $ do
        result <- day3Part2 
        result `shouldBe` 3952146825 
       it "all valid passports are" $ do
        result <- day4
        result `shouldBe` 256 
       it "all valid passports are (part2)" $ do
        result <- day4Part2
        result `shouldBe` 198 
       it "get bigger seat Id" $ do
        result <- day5 
        result `shouldBe` 883 
       it "get missing Id" $ do
        result <- day5Part2 
        result `shouldBe` 532  
