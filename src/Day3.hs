module Day3 where

import Data.List.Split
import Day2
import Data.Monoid (Product)
import Protolude (atMay)

check :: Int -> String -> Char
check index s = let sSize = (length s) 
                    pos = if index < sSize then index 
                                            else index - (sSize * timesBigger) 
                                            where timesBigger = (index `div` sSize)
                in s !! pos  

traverseLeft3Down1Map :: [String] -> [Char] 
traverseLeft3Down1Map lines = f (tail lines) 3
            where f [] _ = []
                  f (l:xl) x = (check x l) : f xl (x+3) 


numberOfThreesEncountered :: [String] -> Int 
numberOfThreesEncountered =
    count '#' . traverseLeft3Down1Map

traverseLeftDownMap :: Int -> Int -> [String] -> [Char] 
traverseLeftDownMap left down lines = f (tail lines) left down 
            where 
                  f xl x y 
                    | y > length xl = []
                    | otherwise = (check x $ xl !! (y-1)) : f xl (x+left) (y+down) 

numberOfThreesEncountered2 :: Int -> Int -> [String] -> Int 
numberOfThreesEncountered2 x y =
    count '#' . traverseLeftDownMap x y

productEnconuteredThressWithDifferentsPatterns :: [(Int,Int)] -> [String] -> Int
productEnconuteredThressWithDifferentsPatterns patterns map = 
        foldr productPatterns 1 patterns   
            where productPatterns (x,y) = (*) $ numberOfThreesEncountered2 x y map 
