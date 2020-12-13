module Day5Spec where


import Test.Hspec
import Day5


spec :: Spec
spec = 
    describe "Day5" $ do
        it "find row as binary space partitioning" $
            findRow "FBFBBFF" `shouldBe` 44 
        it "find column as binary space partitioning" $
            findColumn "RLR" `shouldBe` 5 
        it "upper half" $ do
           uHalf 63 `shouldBe` 32 
           uHalf 64 `shouldBe` 32 
           uHalf 65 `shouldBe` 33
           uHalf 7 `shouldBe` 4 
           uHalf 3 `shouldBe` 2 
        it "parse strings to Seat positions" $
           parseSeats [ "BFFFBBFRRR"
                      , "FFFBBBFRRR"
                      , "BBFFBBFRLL"
                      ]  
           `shouldBe`
           [ SeatPos 70 7
           , SeatPos 14 7
           , SeatPos 102 4
           ]
        it "calculate seatIds" $
           getSeatId <$> parseSeats [ "BFFFBBFRRR"
                      , "FFFBBBFRRR"
                      , "BBFFBBFRLL"
                      ]  
           `shouldBe`
           [ 567
           , 119
           , 820
           ]




