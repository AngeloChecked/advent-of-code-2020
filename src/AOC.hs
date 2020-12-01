module AOC where

import Day1 

day1 = do 
     expenses <- freadInt "resources/expense.txt" 
     return $ product2020Combinations expenses
