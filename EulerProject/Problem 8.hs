import Data.Char (digitToInt)
import Data.List (tails)

problem8 a b = maximum $ map product $ map (map digitToInt) $ problem8' (show a) b
               where 
               problem8' w b = if b == length (take b w) then (take b w) : problem8' (tail w) b else []

problem82 a b = maximum $ map product $ map (map digitToInt) $ problem82' (show a) b 
                where 
                problem82' w b = filter (\x -> length x == b) $ map (take b) (tails w) 



