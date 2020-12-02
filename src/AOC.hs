module AOC where

import Day1 

day1 = do 
     expenses <- freadInt "resources/expense.txt" 
     return $ product2020Combinations expenses

day1Part2 = do
    expenses <- freadInt "resources/expense.txt" 
    return $ product2020Combinations3 expenses

