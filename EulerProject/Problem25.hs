import Data.List (find, elemIndex)

fibonnaci = 1 : 1 : help 1 1
        where 
        help x y = (x+y) : help (x+y) x


problem25 = 1000 `elemIndex` [length $ show x | x <- fibonnaci] 

