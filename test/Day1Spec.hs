module Day1Spec where

import Day1
import Test.Hspec

spec :: Spec
spec = 
    describe "fiz" $
        it "buzz" $
            foo `shouldBe` "fuzz"

