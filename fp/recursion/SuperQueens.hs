
queens :: Int -> Int
queens = pick <*> poss
  where
    poss n = (,) <$> [1..n] <*> [1..n]

    pick 0 _ = 1
    pick _ [] = 0
    pick n ps = sum [pick (n-1) (elim p ps) | p <- ps]

    elim (i,j) = filter bads
      where
        bads (x,y) = x /= i && y /= j
                && (abs (x-i) == abs (y-j))
                && ((x,y) `notElem` [(i `f` dx, j `f` dy) | (dx,dy) <- [(2,1), (1,2)], f <- [(+),(-)]])

main = print . queens =<< readLn
