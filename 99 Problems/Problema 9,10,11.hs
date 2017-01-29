data NovoTipo b = Single b | Multiple Int b deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = takeWhile (== head xs) xs  : (pack $ dropWhile (== head xs) xs)


encodeMod :: (Eq a) => [a] -> [NovoTipo a]
encodeMod = map f . pack
            where
            f xs = if length xs == 1 then Single (head xs) else Multiple (length xs) (head xs)



