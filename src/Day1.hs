
module Day1 where
import System.IO
import Control.Monad

freadInt :: String -> IO [Int]
freadInt fileName = do  
        contents <- readFile fileName 
        return $ map readInt . words $ contents 

readInt :: String -> Int 
readInt = read

allCombinations :: [a] -> [(a,a)]
allCombinations inputs = f inputs
    where 
        f [] = []
        f (x:xs) = g xs ++ f xs 
            where  
                g [] = []
                g (y:ys) = (x,y) : g ys 

                   
productT :: Num a => (a,a) -> ((a,a),a)
productT x@(a,b) = (x,a*b) 
    
filter2020 :: (Num a, Eq a) => [(a,a)] -> [(a,a)]
filter2020 = filter (\(a,b) -> (a+b) == 2020)

product2020Combinations :: (Num a, Eq a) => [a] -> [((a,a),a)]
product2020Combinations = map productT 
                          . filter2020
                          . allCombinations

