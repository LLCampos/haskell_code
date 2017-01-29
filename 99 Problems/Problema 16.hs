import Data.List

problem16 :: (Eq a) => [a] -> Int -> [a]
problem16 xs b 
          | b <= (length xs) = (delete (last bocado) bocado) ++ (problem16 (drop b xs) b)
          | b > (length xs) = xs       
                   where           
                   bocado = take b xs




 