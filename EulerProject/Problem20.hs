import Data.Char (digitToInt)

factorial :: Integer -> Integer
factorial 1 = 1 
factorial x = x * factorial (x-1)

problem20 x = sum $ map digitToInt $ show $ factorial x 