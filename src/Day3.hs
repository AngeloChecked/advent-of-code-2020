module Day3 where

import Data.List.Split
import Day2

check :: Int -> String -> Char
check index s = let sSize = (length s) 
                    pos = if index <= sSize then index 
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
