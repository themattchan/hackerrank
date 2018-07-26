{-# OPTIONS_GHC -O2 #-}
queens :: Int -> Int
queens = pick . poss
  where
    poss n = [ [(row,col) | col <- [1..n]] | row <- [1..n] ]

    pick :: [[(Int,Int)]] -> Int
    pick [] = 0
    pick [p] = length p
    pick (p:ps) = sum [ pick (map (elim x) ps) | x <- p]

    elim (i,j) = filter goods
      where
        goods (x,y) = not $
          x == i -- same row
          || y == j -- same col
          || (abs(x-i) == abs(y-j)) -- diagonal
          -- L shape
          || ((x,y) `elem` [(i `f` dx, j `g` dy) | (dx,dy) <- [(2,1), (1,2)], f <- [(+),(-)], g <- [(+),(-)]])

main = print . queens =<< readLn
