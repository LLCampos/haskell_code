problem48 :: Integer -> Integer 
problem48 x = read $ reverse $ take 10 $ reverse $ show $ help [1..x]
            where
            help [] = 0 
            help (y:ys) = y^y + help ys
