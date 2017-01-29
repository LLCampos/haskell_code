import Data.Char

wword :: Int -> String 
wword 1000 = "onethousand"
wword x 
 | (length $ show x) == 1 = units x 
 | (length $ show x) == 2 = dezenas x 
 | (length $ show x) == 3 = centenas x 

units :: Int -> String
units 0 = ""
units 1 = "one" 
units 2 = "two" 
units 3 = "three" 
units 4 = "four"
units 5 = "five"
units 6 = "six"
units 7 = "seven"
units 8 = "eight"
units 9 = "nine"

dezenas :: Int -> String 
dezenas 10 = "ten"
dezenas 11 = "eleven"
dezenas 12 = "twelve"
dezenas 13 = "thirteen"
dezenas 14 = "fourteen"
dezenas 15 = "fifteen"
dezenas 16 = "sixteen"
dezenas 17 = "seventeen"
dezenas 18 = "eighteen"
dezenas 19 = "nineteen"
dezenas x 
  | (head $ show x) == '2' = "twenty" ++ units (x-20)
  | (head $ show x) == '3' = "thirty" ++ units (x-30)
  | (head $ show x) == '4' = "forty" ++ units (x-40)
  | (head $ show x) == '5' = "fifty" ++ units (x-50)
  | (head $ show x) == '6' = "sixty" ++ units (x-60)
  | (head $ show x) == '7' = "seventy" ++ units (x-70)
  | (head $ show x) == '8' = "eighty" ++ units (x-80)
  | (head $ show x) == '9' = "ninety" ++ units (x-90)
 
centenas x = (units $ help3 x) ++ "hundred" ++ centenas' x
            where 
            centenas' x = if help x /= 0
                          then "and" ++ dezenas (x - (help3 x) * 100)
                          else if help2 x == 0 then "" else "and" ++ (units $ help2 x)
            help x = digitToInt $ (show x) !! 1
            help2 x = digitToInt $ last $ show x
            help3 x = digitToInt $ head $ show x 
  

problem17 xs = length $ concatMap wword xs 