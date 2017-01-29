import Data.List

isPalindrome :: (Show a) => a -> Bool
isPalindrome x = (show x) == reverse (show x) 

problemfour :: (Show a, Real a, Enum a) =>  Int -> Maybe a         
problemfour y = find (help $ allxDigit y) (allPalindromes y)
           where 
           allxDigit x =  [10^(x-1)..(10^x)-1]
           allPalindromes x = reverse $ filter isPalindrome [(head $ allxDigit x)^2..(last $ allxDigit x)^2]
           z y = map realToFrac y 
           f x y = intersect y $ map (realToFrac x/) (z y)
           help y x = if length(f x y) > 0 then True else False

problemfour' = maximum [x * y | x <- [100..999] , y <- [x..999] , isPalindrome (x*y)]





