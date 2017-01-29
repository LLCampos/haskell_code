import Data.Maybe (fromJust) 
import Data.List 

problem18 :: [[Int]] -> Int 
problem18 (x:xs) = head x + problem18' xs 1
            where 
            problem18' [] _ = 0 
            problem18' (x:xs) y = dois x y + problem18' xs (help x y) 
            dois x y = maximum $ [(x !! (y-1)),(x !! y)]
            help x y = 1 + (fromJust $ elemIndex (dois x y) x )


main = do 
   args <- getArgs 
   contents <- readFile args 
   putStrLn (lines contents)