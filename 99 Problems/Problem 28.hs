import Data.List
import Data.Function (on) 
import Data.Ord (comparing)

problem28 :: Ord a => [[a]] -> [[a]]
problem28 xs = map snd $ sort [(length x, x) | x <- xs]

problem28' xs = sortBy (\x y -> compare (length x) (length y)) xs 

problem28'' xs = sortBy (compare `on` length) xs 

problem28''' xs = sortBy (comparing length) xs


problem282 :: [[a]] -> [[a]]
problem282 xs = concat $ problem28' $ groupBy ((==) `on` length) (problem28' xs)











