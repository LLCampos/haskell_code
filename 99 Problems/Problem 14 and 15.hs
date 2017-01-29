dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

dupli2 :: [a] -> [a]
dupli2 = foldr (\x acc -> x : x : acc) []

dupli3 :: [a] -> [a]
dupli3 = concatMap help 
        where 
        help x = [x,x]

problem15 :: [a] -> Int -> [a]
problem15 xs a = concat $ problem15x (help1 xs) a 
               where 
               help1 = map (\x -> [x]) 

problem15x :: [[a]] -> Int -> [[a]]
problem15x xs 1 = xs
problem15x xs a = problem15x (help2 xs) (a-1)
               where
               help2 xs = map (\x -> (head x) : x) xs


problem15' :: [a] -> Int -> [a]
problem15' [] _ = []
problem15' (x:xs) a = replicate a x ++ problem15' xs a



