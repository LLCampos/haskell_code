data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem a)   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


flatten2 :: NestedList a -> [a]
flatten2 a = concatMap f [a]
             where 
             f (Elem a) = a 
             f (List a) = flatten2 (a)


