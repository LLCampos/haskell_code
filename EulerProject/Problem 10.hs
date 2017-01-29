import Data.List
import qualified Data.Map as M

listprimes' :: [Int] -> [Int]
listprimes' [] = []
listprimes' (x:xs) = x : listprimes' [y | y <- xs , (mod y x) /= 0]

listprimes :: Int -> [Int]
listprimes x = listprimes' [2..x]

problemten x = sum (listprimes x) 



isprime :: (Integral i) => i -> Bool
isprime n = isprime_ n primes
  where isprime_ n (p:ps)
          | p*p > n        = True
          | n `mod` p == 0 = False
          | otherwise      = isprime_ n ps

primes :: (Integral i) => [i]
primes = 2 : filter isprime [3,5..]