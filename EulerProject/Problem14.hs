import Data.List
import Data.Function (on)

problem14 :: Int -> Int 
problem14 x = fst $ maximumBy (compare `on` snd) $ map help [1..(x-1)]
            where 
            a = map help [1..(x-1)]
            help y = (y,length $ collatz y)



collatz :: Int -> [Int]
collatz 1 = [1]
collatz x = x : collatz (help x)
     where
     help x 
         | odd x = 3 * x + 1
         | even x = x `div` 2 


collatz' :: Integer -> [Integer]
collatz' 1 = []
collatz' n
    | odd n = 3*n+1:collatz' (3*n+1)
    | even n = div n 2:collatz' (div n 2)

chains = [collatz' x | x <- [1..]]

problem14'' = map length $ take 1000000 chains
