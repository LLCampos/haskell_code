import Data.List (permutations) 



problem31 = [(x+x) | x <- [0,1,2,5,10,20,50,100,200] , (x+x) == 200 ]