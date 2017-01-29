import Data.List

listprimes (x:xs) = x : listprimes [y | y <- xs , (y `mod` x) /= 0]

problem39 x y = intersect [x..y] $ listprimes [2..y]