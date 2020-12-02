module Day1Spec where
import Day1
import Test.Hspec

spec :: Spec
spec = 
    describe "day 1" $ do
        it "read expense file" $ do 
            content <- freadInt "resources/expense.txt" 
            content `shouldStartWith` [1782,1344,1974,1874,1800,1973,1416,1952,1982,1506]
        it "all combinations without duplicate and previous values" $ do
            let combinations = allCombinations [5,10,11,15]
            combinations `shouldBe` [(5,10),(5,11),(5,15),(10,11),(10,15),(11,15)]
            combinations `shouldNotContain` [(5,5)]
            combinations `shouldNotContain` [(10,5)]
            combinations `shouldNotContain` [(10,10)]
            combinations `shouldNotContain` [(11,5)]
            combinations `shouldNotContain` [(11,10)]
            combinations `shouldNotContain` [(11,11)]
            combinations `shouldNotContain` [(15,5)]
            combinations `shouldNotContain` [(15,10)]
            combinations `shouldNotContain` [(15,11)]
            combinations `shouldNotContain` [(15,15)] 
        it "get all combianetions of three numbers" $
            allCombinations3 [2,3,4] `shouldBe` [[2,2,2],[2,2,3],[2,2,4],[2,3,2],[2,3,3],[2,3,4],[2,4,2],[2,4,3],[2,4,4],[3,2,2],[3,2,3],[3,2,4],[3,3,2],[3,3,3],[3,3,4],[3,4,2],[3,4,3],[3,4,4],[4,2,2],[4,2,3],[4,2,4],[4,3,2],[4,3,3],[4,3,4],[4,4,2],[4,4,3],[4,4,4]] 
        it "product two number element of tuple and return inputs and result" $ 
            productT (5,5) `shouldBe` ((5,5),25)
        it "keep only when sum of tuple elements is 2020" $   
            filter2020 [(5,5),(2000,20)] `shouldBe` [(2000,20)]
        it "product all combiantion of 2 numbers that sum to 2020" $
            product2020Combinations [1721,979,366,299,675,1456] `shouldBe` [((1721,299), 514579)]
        it "product all combiantion of 3 numbers that sum to 2020" $
            product2020Combinations3 [1721,979,366,299,675,1456] `shouldBe` [241861950]





