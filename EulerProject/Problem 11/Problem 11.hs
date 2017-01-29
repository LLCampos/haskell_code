import System.IO
import Data.List (transpose) 
import Data.Matrix (fromLists) 

problem11 xs = fromLists $ grelha xs

grelha :: String -> [[Int]]
grelha xs = map ((map read) . words) $ lines xs

suphorizontal :: [[Int]] -> Int 
suphorizontal xs = maximum $ [maximum $ map maximum $ map horizontal $ xs , maximum $ map maximum $ map horizontal $ map reverse $ xs]

supvertical :: [[Int]] -> Int 
supvertical xs = suphorizontal $ transpose $ xs

horizontal :: [Int] -> [Int]
horizontal [] = []
horizontal xs = (product $ take 4 xs) : horizontal (drop 1 xs) 



main = do 
       handle <- openFile "grelha.txt" ReadMode
       contents <- hGetContents handle
       print (problem11 contents) 
       hClose handle
      