main = getLine >>= putStrLn . finalise . fix go
  where
    finalise [] = "Empty String"
    finalise s  = s

    fix f x = let x' = f x in if x' == x then x else fix f x'

    go (x:y:zs) | x == y = go zs
                | otherwise = x : go (y:zs)
    go xs = xs
