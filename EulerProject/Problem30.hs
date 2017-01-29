import Data.Char

problem30 :: Int -> [Int]
problem30 x = drop 1 $ [ y | y <- [1..] , y == power y x]


power n x = sum $ map (\y -> y^x) $ map digitToInt $ show n 

teste x = takeWhile (help x) [10..]
          where
          help x y = (length $ show y) >= (length $ show $ power y x)