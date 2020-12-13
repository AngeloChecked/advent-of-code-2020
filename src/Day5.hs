module Day5 where

import Data.Map (Map, fromList, insert, foldrWithKey)
import Data.List (find)

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

data SeatPos = SeatPos { row :: Int, column :: Int } deriving (Show, Eq, Ord)

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

missingSeatInTable :: [SeatPos] -> [SeatPos]
missingSeatInTable pos = let allFillSeat = foldr (\pos acc -> insert pos True acc) seatTable pos    
                         in lookupKey False allFillSeat  
  
lookupKey :: Eq v => v -> Map k v -> [k]
lookupKey val = foldrWithKey go [] 
                    where go key value found = if value == val
                                               then key:found
                                               else found 

seatTable :: Map SeatPos Bool
seatTable = fromList $ f 127 7 
    where f rows columns = [ (SeatPos x y, False) | x <- [0..rows], y <- [0..columns]] 

ignoreStartEndSeats :: [SeatPos] -> [SeatPos]
ignoreStartEndSeats = removeEnd 0 0 127 7 . removeStart 0 0 127 7   

removeStart :: Int -> Int -> Int -> Int -> [SeatPos] -> [SeatPos]
removeStart irSta icSta irEnd icEnd pos = f irSta icSta pos 
        where 
            f rSta cSta xxs@(x:xs) 
                | cSta <= icEnd && rSta <= irEnd 
                               = if (SeatPos rSta cSta == x) 
                                 then f rSta (cSta+1) xs        
                                 else xxs
                | rSta <= irEnd = f (rSta+1) icSta xxs        
                | otherwise = [] 

removeEnd :: Int -> Int -> Int -> Int -> [SeatPos] -> [SeatPos]
removeEnd irSta icSta irEnd icEnd pos = reverse $ f irEnd icEnd (reverse pos)
        where 
            f rEnd cEnd xxs@(x:xs) 
                | cEnd >= icSta && rEnd >= irSta 
                               = if (SeatPos rEnd cEnd == x) 
                                 then f rEnd (cEnd-1) xs        
                                 else xxs
                | rEnd >= irSta = f (rEnd-1) icEnd xxs        
                | otherwise = [] 
