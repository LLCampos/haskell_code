import Data.List (nub, sort)



combinations a list = nub $ map sort $ filter (\x -> length x == 3) $ map nub $ g a list
                      where
                      g a list = foldl (\acc x -> concatMap (help list) acc) [[]] [1..a]
                      help list x = map (\y -> y : x) list 
                    

