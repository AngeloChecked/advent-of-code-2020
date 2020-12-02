module Day2 where

import Data.Monoid
import Data.List.Split
import Data.List.NonEmpty as NonEmpty (xor, fromList) 
import Day1

fread :: String -> IO [String]
fread fileName = do  
        contents <- readFile fileName 
        return $ lines $ contents 

data PasswordToValidate = PasswordToValidate { minRange :: Int
                                             , maxRange :: Int
                                             , char :: Char
                                             , password :: String 
                                             } deriving (Show,Eq)

count :: Eq a => a -> [a] -> Int
count a = length . filter (a==)  

validPassword :: PasswordToValidate -> Bool
validPassword a = let numberOfMandatoryChar = count (char a) (password a) in
                      getAll $ foldMap All [ numberOfMandatoryChar <= (maxRange a)
                                           , numberOfMandatoryChar >= (minRange a)
                                           ]   
                                                                                            
passwordToValidateFromString :: String -> PasswordToValidate
passwordToValidateFromString s = let splitRangesAndPassword = splitOn ":" s
                                     password = (tail . last) splitRangesAndPassword
                                     rangesAndChar = splitOn " " $ head splitRangesAndPassword
                                     ranges = head rangesAndChar 
                                     char = (head . last) rangesAndChar
                                     [min,max] = readInt <$> (splitOn "-" ranges)
                                 in PasswordToValidate  min max char password   


allValidPassword :: [PasswordToValidate] -> [PasswordToValidate]
allValidPassword = filter validPassword

-- part2

validPassword2 :: PasswordToValidate -> Bool
validPassword2 a =  let c = (==(char a)) . (\i -> (password a)!!(i-1)) <$> [minRange a, maxRange a] 
                    in xor $ NonEmpty.fromList c 

allValidPassword2 :: [PasswordToValidate] -> [PasswordToValidate]
allValidPassword2 = filter validPassword2
                 
                
