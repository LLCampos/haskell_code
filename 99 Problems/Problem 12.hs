import Data.List (concatMap)

data NovoTipo b = Single b | Multiple Int b deriving (Show)

f :: NovoTipo b -> [b]
f (Multiple i j) = replicate i j 
f (Single j) = [j]

decodeMod :: [NovoTipo b] -> [b]
decodeMod xs = concatMap f xs 
            
