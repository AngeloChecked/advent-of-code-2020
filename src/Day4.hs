{-# LANGUAGE TypeApplications #-}
module Day4 where

import Data.List.Split (splitOn)
import Control.Monad

data Passport = Passport { byr :: String
                         , iyr :: String
                         , eyr :: String
                         , hgt :: String
                         , hcl :: String
                         , ecl :: String
                         , pid :: String
                         , cid :: Maybe String
                         } deriving (Show, Eq)

allp :: [(a -> Bool)] -> a -> Bool
allp fs a = foldr (\c -> (&&c)) True $ fs <*> pure a

validDate :: [Int -> Bool] -> String -> Maybe String 
validDate validators value = do
        guard (length value == 4) 
        let valueNumber = (read @Int) value 
        guard (allp validators valueNumber) 
        return $ value 

takeLast :: Int -> [a] -> ([a],[a])
takeLast n xs = let i = (length xs)-n in (take i xs, drop i xs)

validMeasure :: String -> [Int -> Bool] -> String -> Maybe String
validMeasure m validators value = do
    let (misure, unit) = takeLast 2 value
    guard (unit == m) 
    let misureNumber = (read @Int) misure 
    guard (allp validators misureNumber) 
    return value 

validMeasures :: String -> Maybe String
validMeasures = do
    a <- validMeasure "in" [(>=59),(<=76)] 
    b <- validMeasure "cm" [(>=150),(<=193)]     
    pure $ msum [a,b] 

validColor :: String -> Maybe String
validColor color = do
            let firstChar = take 1 color 
                colorBody = drop 1 color 
                cleredBody = filter (\c -> any (c==) $ ((!!0) . show <$> [0..9]) ++ ['a'..'f']) colorBody 
            guard (firstChar == "#") 
            guard (length cleredBody == 6) 
            pure color 

validPid :: String -> Maybe String
validPid pid = do
            let cleredPid = filter (\c -> any (c==) $ ((!!0) . show <$> [0..9])) pid 
            guard (length (filter (=='0') pid) < 9)
            guard (length cleredPid == 9) 
            pure pid 

validEyeColor :: String -> Maybe String
validEyeColor eColor = do
         guard $ any (==eColor) ["amb","blu","brn","gry","grn","hzl","oth"]
         pure eColor

fstOrNull :: [a] -> Maybe a
fstOrNull (a:[]) = Just a
fstOrNull _ = Nothing

removeKey :: String -> String
removeKey = tail . dropWhile (/=':')

parsePassport :: [String] -> Maybe Passport
parsePassport batch = let  startWith = \key -> filter (\str -> key == ((str!!) <$> [0,1,2]))
                           findByKey = \key -> (fmap removeKey) $ fstOrNull $ startWith key batch
                      in Passport <$> (findByKey "byr")
                                  <*> (findByKey "iyr") 
                                  <*> (findByKey "eyr") 
                                  <*> (findByKey "hgt") 
                                  <*> (findByKey "hcl") 
                                  <*> (findByKey "ecl") 
                                  <*> (findByKey "pid") 
                                  <*> (Just $ findByKey "cid")

parsePassport2 :: [String] -> Maybe Passport
parsePassport2 batch = let  startWith = \key -> filter (\str -> key == ((str!!) <$> [0,1,2]))
                            findByKey = \key -> (fmap removeKey) $ fstOrNull $ startWith key batch
                      in Passport <$> (validDate [(>=1920),(<=2002)] =<< findByKey "byr")
                                  <*> (validDate [(>=2010),(<=2020)] =<< findByKey "iyr") 
                                  <*> (validDate [(>=2020),(<=2030)] =<< findByKey "eyr") 
                                  <*> (validMeasures =<< findByKey "hgt") 
                                  <*> (validColor =<< findByKey "hcl") 
                                  <*> (validEyeColor =<< findByKey "ecl") 
                                  <*> (validPid =<< findByKey "pid") 
                                  <*> (Just $ findByKey "cid")

splitPassportsRawFromBatch :: [String] -> [[String]]
splitPassportsRawFromBatch = splitOn [""]

splitSpaceAndFlat :: [[String]] -> [[String]]
splitSpaceAndFlat = fmap $ foldMap (splitOn " ")  

catMaybes :: [Maybe a] -> [a]
catMaybes ls = do 
               Just x <- ls 
               return x

parseAllValidPassports :: [[String]] -> [Passport]
parseAllValidPassports = catMaybes . map parsePassport 

parseAllValidPassports2 :: [[String]] -> [Passport]
parseAllValidPassports2 = catMaybes . map parsePassport2 

