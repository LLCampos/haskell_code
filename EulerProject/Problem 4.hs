import Data.List


isPalindrome x = ("x" == (reverse "x") )



allxDigit :: (Integral a) => a -> [a]
allxDigit x = [10^(x-1)..(10^x)-1]


listPalindromes x = reverse $ filter isPalindrome (maxmin x) 
          where 
          maxmin x = [(primeiro x)^2..(ultimo x)^2]
          primeiro x = head (allxDigit x)
          ultimo x = last (allxDigit x) 



              
             
              
                                         
                              