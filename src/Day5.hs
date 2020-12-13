module Day5 where

uHalf :: Int -> Int
uHalf n = div n 2 + mod n 2 

findRow :: [Char] -> Int
findRow = findSeat (SeatDriver 'B' 'F' 0 127) 

findColumn :: [Char] -> Int
findColumn = findSeat (SeatDriver 'R' 'L' 0 7) 

data SeatDriver = SeatDriver { fromStartChar :: Char
                             , fromEndChar :: Char
                             , start :: Int
                             , end :: Int
                             } deriving (Show)

data SeatPos = SeatPos { row :: Int, column :: Int } deriving (Show, Eq)

parseSeats :: [String] -> [SeatPos]
parseSeats s = do
                coo <- s
                let l = (length coo)-3 
                    row = findRow (take l coo) 
                    column = findColumn (drop l coo)
                return $ SeatPos row column 
            
getSeatId :: SeatPos -> Int 
getSeatId pos = (row pos) * 8 + (column pos)



findSeat :: SeatDriver -> [Char] -> Int
findSeat driver pos = f pos (start driver) (end driver) 
        where
            u = fromStartChar driver
            l = fromEndChar driver
            f (s:[]) back front    
                                | s==u = if (back>front) then back else front 
                                | s==l = if (back<front) then back else front 
                                | otherwise = error "impossible" 
            f (s:xs) back front 
                                | s==u = f xs (back+(uHalf remainingSeats)) front      
                                | s==l = f xs back (front-(uHalf remainingSeats))
                                | otherwise = error "impossible" 
                                where remainingSeats = front-back
