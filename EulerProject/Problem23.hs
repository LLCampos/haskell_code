import Data.List

spd :: Integer -> Integer
spd x = sum $ (delete x) $ nub $ concat $ [ [y,(div x y)] | y <- [1..(ceiling $ sqrt $ fromIntegral x)] , x `mod` y == 0]

abudantNumbers :: [Integer]
abudantNumbers = [x | x <- [1..28123] , spd x > x ]


sumAbudantNumbers = [(x+y) | x <- abudantNumbers , y <- abudantNumbers , x > y , (x+y) < 28123]



problem23 = [x | x <- [1..28123] , not $ elem x $ takeWhile (<= x) sumAbudantNumbers]








