len :: [a] -> Int
len lst = foldl (\a x->1+a) 0 lst
