
import Data.List

spd x = sum $ (delete x) $ nub $ concat $ [ [y,(div x y)] | y <- [1..(ceiling $ sqrt $ fromIntegral x)] , x `mod` y == 0]

amicable =  sum $ takeWhile (<10000) [x | x <- [1..] , x == spd (spd (x)) , x /= spd x]





























eprime :: Integral a => a -> Bool 
eprime x = if any (==0) (map (mod x) [2..x-1])  then False else True

listprime :: Integral a => a -> [a]
listprime 2 = [2]
listprime x = filter eprime [2..x]
    
infiprime :: Integral a => [a]
infiprime = filter eprime [2..]


p = filter eprime [(2^(n-m)+1) * (2^m - 1)| m <- [1..] , n <- [m..]]
q = filter eprime [(2^(n-m)+1) * (2^n - 1)| m <- [1..] , n <- [m..]]
r = filter eprime [((2^(n-m)+1))^2 * ((2^(m+n)) - 1) | m <- [1..] , n <- [m..]]

