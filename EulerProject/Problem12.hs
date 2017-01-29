import Data.List
import Data.Maybe (fromJust) 


triangleNumberList = [ sum [1..x] | x <- [1..]]

factors x = length $ nub $ concat $ [ [y,(div x y)] | y <- [1..(ceiling $ sqrt $ fromIntegral x)] , x `mod` y == 0]

problem12 b = fst $ fromJust $ find (\y -> snd y > b) [(x, factors x) | x <- triangleNumberList]




