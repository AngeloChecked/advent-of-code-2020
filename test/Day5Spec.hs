module Day5Spec where


import Test.Hspec
import Day5
import Data.Map (fromList, insert)


spec :: Spec
spec = do
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
    describe "Day5 Part2" $ do
        it "find all kay with given value in map" $
            lookupKey True (fromList [(1,True),(2,False),(3,True)]) `shouldBe` [1,3]
        it "get all missing position in given table" $
            missingSeatInTable testSeatsInputWithout100row 
            `shouldBe` [ SeatPos 100 0
                       , SeatPos 100 1
                       , SeatPos 100 2
                       , SeatPos 100 3
                       , SeatPos 100 4
                       , SeatPos 100 5
                       , SeatPos 100 6
                       , SeatPos 100 7
                       ]      
        it "ignore all seats from start and end" $ 
            ignoreStartEndSeats [ SeatPos 0 0, SeatPos 0 1, SeatPos 0 2, SeatPos 0 3, SeatPos 0 4, SeatPos 0 5, SeatPos 0 6, SeatPos 0 7
                                , SeatPos 1 0, SeatPos 1 1, SeatPos 1 2, SeatPos 1 3, SeatPos 1 4, SeatPos 1 5, SeatPos 1 6, SeatPos 1 7
                                , SeatPos 5 5
                                , SeatPos 100 5
                                , SeatPos 126 0, SeatPos 126 1, SeatPos 126 2, SeatPos 126 3, SeatPos 126 4, SeatPos 126 5, SeatPos 126 6, SeatPos 126 7
                                , SeatPos 127 0, SeatPos 127 1, SeatPos 127 2, SeatPos 127 3, SeatPos 127 4, SeatPos 127 5, SeatPos 127 6, SeatPos 127 7
                                ] `shouldBe`
            [SeatPos 5 5, SeatPos 100 5]

testSeatsInputWithout100row = [ (SeatPos x y) | x <- [0..99]++[101..127], y <- [0..7]] 

