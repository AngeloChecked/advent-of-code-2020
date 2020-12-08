module Day4 where

import Data.List.Split (splitOn)

data Passport = Passport { byr :: String
                         , iyr :: String
                         , eyr :: String
                         , hgt :: String
                         , hcl :: String
                         , ecl :: String
                         , pid :: String
                         , cid :: Maybe String
                         } deriving (Show, Eq)

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

