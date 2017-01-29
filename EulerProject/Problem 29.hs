import Data.List

problem29 :: Integer -> Integer -> Int 
problem29 x y = length $ nub $ [ a^b | a <- [x..y] , b <- [x..y]]