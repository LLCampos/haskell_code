import Data.Char (digitToInt) 
import Data.List
import Data.Maybe (fromJust) 

factorial :: Int -> Int
factorial x = product [1..x]

curious :: Int -> Int
curious x = sum $ map factorial $ map digitToInt $ show x

maxd = factorial 9 * ((fromJust $ find (\x -> (length $ show $ factorial 9 * x) < x) [2..])-1)

problem34 = sum [x | x <- [10..maxd] , x == curious x]

