import Data.List

eprime :: Integral a => a -> Bool 
eprime x = if any (==0) (map (mod x) [2..x-1])  then False else True

listprime :: Integral a => a -> [a]
listprime 2 = [2]
listprime x = filter eprime [2..x]
    
infiprime :: Integral a => [a]
infiprime = filter eprime [2..]

listprimes' :: [Int] -> [Int]
listprimes' (x:xs) = x : listprimes' (filter (\p -> p `mod` x /= 0) xs)
           

listprimes :: [Int]
listprimes = listprimes' [2..]


primeFactorization :: Integer -> [Integer]
primeFactorization 1 = []
primeFactorization x = help x : primeFactorization (x `div` (help x))
                       where
                       help x = case find (help1 x) (listprime x) of 
                                Just a -> a 
                                Nothing -> 1 
                       help1 x y = if (mod x y) == 0 then True else False


problem36 :: Int -> [(Int,Int)]
problem36 x = g (f x)
           where 
           f x = group $ sort $ primeFactorization x
           g y = map (\xs -> (head xs , length xs)) y 



