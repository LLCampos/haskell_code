import System.IO
import Data.List (sort, find)
import Data.Maybe (fromJust)

main = do 
       withFile "p022_names.txt" ReadMode (\handle -> do 
       contents <- hGetContents handle
       putStrLn (show $ problem22 contents))
     

tirar :: String -> String
tirar [] = []
tirar (x:xs) = if x == '"' || x == ',' then ' ' : tirar xs else x : tirar xs

problem22 :: String -> Int
problem22 xs =  sum $ zipWith (*) [1..] $ map namescore $ sort $ words $ tirar xs 

alphabetscore :: Char -> Int
alphabetscore a = fst $ fromJust $ find (\x -> snd x == a) $ zip [1..] ['A'..'Z']

namescore :: String -> Int 
namescore name = sum $ map alphabetscore name