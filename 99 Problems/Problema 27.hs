import Data.List (nub, sort)



combinations a list = nub $ map sort $ filter (\x -> length x == 3) $ map nub $ g a list
                      where
                      g a list = foldl (\acc x -> concatMap (help list) acc) [[]] [1..a]
                      help list x = map (\y -> y : x) list 
                    

group' [a,b,c] list = 
where 
la = combinations a list
lb = combinations b list
lc = combinations c list 