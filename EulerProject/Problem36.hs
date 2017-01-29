dectobinary :: Integer -> Integer
dectobinary 0 = 0 
dectobinary n = 10^((length $ a)-1) + dectobinary (n - (last a))
    where 
    a = takeWhile (<= n) elevadosa2
    elevadosa2 = [2^x | x <- [0..]]

isPalindrome xs = (show xs) == reverse (show xs) 


problem36 = sum [x | x <- [0..999999] , isPalindrome x , isPalindrome $ dectobinary x]