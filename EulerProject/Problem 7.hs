import Data.List 


listprimes' :: [Int] -> [Int]
listprimes' (x:xs) = x : listprimes' (filter (\p -> p `mod` x /= 0) xs)
           

listprimes :: [Int]
listprimes = listprimes' [2..]

problemseven :: Int -> Int 
problemseven x = (listprimes) !! x 

