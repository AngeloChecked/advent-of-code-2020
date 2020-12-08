module AOC where

import Day1 
import Day2
import Day3
import Day4

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

day3 = do
    map <- fread "resources/threes.txt" 
    return $ numberOfThreesEncountered map

day3Part2 = do
    map <- fread "resources/threes.txt" 
    return $ productEnconuteredThressWithDifferentsPatterns [(1,1),(3,1),(5,1),(7,1),(1,2)] map

day4 = do
    passports <- fread "resources/passports.txt" 
    return $ length $ parseAllValidPassports (splitSpaceAndFlat $ splitPassportsRawFromBatch passports)  

