import Data.Char (digitToInt)
import Data.List

rotations :: Int -> [Int]
rotations x = map read $  rotations' (cycle $ show x) a
    where
    rotations' ciclo 0 = []
    rotations' ciclo n = (take a $ drop n $ ciclo) : rotations' ciclo (n-1) 
    a = length $ show x 

eprime :: Int -> Bool
eprime x = if any (==0) (map (mod x) [2..x-1])  then False else True

listprimes' :: [Int] -> [Int]
listprimes' [] = []
listprimes' (x:xs) = x : listprimes' (filter (\p -> p `mod` x /= 0) xs)

circularprime :: Int -> Bool
circularprime x = if any (False==) $ map eprime $ rotations x then False else True

primes :: [Int]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors :: Int -> [Int]
primeFactors n = factor n primes
    where
        factor _ [] = []
        factor m (p:ps) | p*p > m        = [m]
                        | m `mod` p == 0 = p : factor (m `div` p) (p:ps)
                        | otherwise      = factor m ps


problem35 n = length $ [x | x <- takeWhile (<n) primes , circularprime x]

