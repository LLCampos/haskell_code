
import Data.List
import Data.Maybe


problemnine z = problemnine' (bah z)
              where 
              bah = [[m,n] | n <- [1..] , m <- [1..n]]
              problemnine' xs = product $ head $ filter (\ [a,b,c] -> a+b+c == z) (map f xs)
              f [m,n] = [(n^2) - (m^2) , 2 * m * n , n^2 + m^2 ]

problemnine2 = product $ fromJust $ find (\a -> sum a == 1000) $ map (\(n, m) -> [n^2 - m^2, 2*m*n, n^2 + m^2]) $ [(x,y) | x <- [1..], y<-[1..x]]




       