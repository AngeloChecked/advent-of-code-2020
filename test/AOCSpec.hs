module AOCSpec where

import AOC 
import Test.Hspec

spec :: Spec
spec = 
    describe "day 1" $
       it "product all numbers combinations in expense file that sum to 2020" $ do
        result <- day1 
        result `shouldBe` [((954,1066),1016964)]
