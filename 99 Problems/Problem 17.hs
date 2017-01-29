
split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split xs a = (help xs a,help2 xs a)


help _ 0 = []
help (x:xs) a = x : help xs (a-1) 
help2 xs 0 = xs
help2 (x:xs) a = help2 xs (a-1) 

