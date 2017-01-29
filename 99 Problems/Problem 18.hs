slice :: [a] -> Int -> Int -> [a]
slice xs a b 
       | a > b = []
       | a <= b = reverse $ drop c (help xs a) 
       where           
       help xs a = reverse $ drop (a-1) xs 
       c = (length xs) - b

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs a 
   | a == 0 = xs 
   | a > 0 = rotate (help xs) (a-1) 
   | a < 0 = rotate (help' xs) (a+1)
   where 
   help xs = tail xs ++ [head xs]
   help' xs = last xs : init xs


removeAt a xs = ([help a xs] , help2 a xs)
      where 
      help a xs = xs !! (a-1)
      help2 a xs = f a xs ++ drop a xs
      f a xs = init $ take a xs 

removeAt' a xs = help $ splitAt a xs
         where 
         help (a,b) = (tail a , (head a) : b)

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs a = help x $ splitAt (a-1) xs 
         where 
         help x (a , b) = a ++ [x] ++ b 

      
range :: Int -> Int -> [Int]
range a b 
      | a > b = [a,(a-1)..b]
      | otherwise = [a..b]

range2 a b = [a..b]
         
