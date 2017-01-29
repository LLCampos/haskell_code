import Data.List

problemfive :: (Integral a) => a -> a 
problemfive x = case find evendevisible [x,(x*2)..] of
                (Just n) -> n 
                where 
                caracas y z = if mod y z == 0 then True else False
                evendevisible y = all (caracas y) [1..x]
 

problemfive2 :: (Integral a) => a -> Maybe a 
problemfive2 x = find (f x) [1..]
               where
               f x y = all (==0) $ map (mod y) [1..x]


problemfive3 :: (Integral a) => a -> a  
problemfive3 x = case find (f x) [x,x+x..] of 
               (Just n) -> n 
               where
               f x y = if any (/=0) $ map (mod y) [1..x] then False else True 

problemfive4 :: Int -> Int 
problemfive4 x = case find (test x) [x,x+x..] of 
                (Just n) -> n 
                where 
                caracas y z = if mod y z == 0 then True else False
                argh y = takeWhile (caracas y) [1..x]
                test x y = if (length (argh y)) == x then True else False 


eprime :: Integral a => a -> Bool 
eprime x = if any (==0) (map (mod x) [2..x-1])  then False else True

listprime :: Integral a => a -> [a]
listprime 2 = [2]
listprime x = filter eprime [2..x]
    
primeFactorization :: Int -> [Int]
primeFactorization 1 = []
primeFactorization x = help x : primeFactorization (x `div` (help x))
                       where
                       help x = case find (help1 x) (listprime x) of 
                                Just a -> a 
                                Nothing -> 1 
                       help1 x y = if (mod x y) == 0 then True else False

help :: (Eq a) => [[a]] -> [[a]]
help (x:y:xs) 
     | (head x) == (head y) = help (y:xs) 
     | (head x) /= (head y) = x : help (y:xs)
help (x:[]) = x : []


problemFive5 :: Int -> Int 
problemFive5 x = product $ concat $ help $ sort $ group $ concatMap primeFactorization [2..x]




help2 :: Int -> Int -> Bool
help2 x 1 = True
help2 x y = mod x y == 0 && help2 x (y-1)

problem5 :: Int -> Int -> Int
problem5 x y 
  | help2 x y  = x
  | help2 x y == False = problem5 (x+9699690) y
  | otherwise = 0



     


