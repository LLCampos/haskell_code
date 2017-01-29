import Data.List

eprime :: Integral a => a -> Bool 
eprime x = if any (==0) (map (mod x) [2..x-1])  then False else True

problemthirtytwo :: Int -> Int -> Int 
problemthirtytwo x y = last $ intersect (help x) (help y)
                      where
                      help x = filter (f x) [1..x] 
                      f x z = x `mod` z == 0

problem32 :: Int -> Int -> Int 
problem32 x y 
       | x == 0 || y == 0  = max x y 
       | x > y = problem32 y (mod x y) 
       | x < y = problem32 x (mod y x) 
       | x == y = x 

problem33 :: Int -> Int -> Bool
problem33 x y = problem32 x y == 1

problem34 :: Int -> Int 
problem34 m = length $ filter (problem33 m) [1..m]

listprime :: Integral a => a -> [a]
listprime 2 = [2]
listprime x = filter eprime [2..x]
    
infiprime :: Integral a => [a]
infiprime = filter eprime [2..]


primeFactorization :: Int -> [Int]
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


problem37 :: Int -> Int 
problem37 x = product $ problem37' $ problem36 x 
          where 
          problem37' xs = map (\(x,y) -> (x-1) * x ^ (y-1)) xs 