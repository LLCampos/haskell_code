listprimes' :: [Int] -> [Int]
listprimes' [] = []
listprimes' (x:xs) = x : listprimes' (filter (\p -> p `mod` x /= 0) xs)



listprimes :: Int -> [Int]
listprimes x = 1: listprimes' [2..x]

goldbach :: Int -> (Int, Int)
goldbach x 
       | odd x = error "Odd x" 
       | x <= 2 = error "x tem de ser mais que 2"
       | otherwise = head [(a,b) | a <- (drop 1 $ listprimes x) , b <- (drop 1 $ listprimes x) , a < b, a + b == x]


goldbachlist :: Int -> Int -> [(Int,Int)]
goldbachlist a b 
          | a < b = map goldbach $ filter even [a..b]
          | otherwise = map goldbach $ filter even [b..a]