module AOC where

import Day1 
import Day2

day1 = do 
     expenses <- freadInt "resources/expense.txt" 
     return $ product2020Combinations expenses

day1Part2 = do
    expenses <- freadInt "resources/expense.txt" 
    return $ product2020Combinations3 expenses

day2 = do
    passwordsToValidate <- fread "resources/passwords.txt" 
    return $ length . allValidPassword $ passwordToValidateFromString <$> passwordsToValidate

day2Part2 = do
    passwordsToValidate <- fread "resources/passwords.txt" 
    return $ length . allValidPassword2 $ passwordToValidateFromString <$> passwordsToValidate
