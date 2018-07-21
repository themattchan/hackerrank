import Control.Monad
import qualified Data.Array as A

lcs xs ys = a A.! (0,0)
  where
    a = A.array ((0,0), (lxs, lys)) $ inits ++ go

    inits =  [((i,lys), 0) | i <- [0..lxs]] ++ [((lxs,j), 0) | j <- [0..lys]]

    go = [ ((i,j),
            if x == y
            then 1 + a A.! (i+1,j+1)
            else max (a A.! (i+1,j)) (a A.! (i,j+1)))
         | (i,x) <- zip [0..] xs, (j,y) <- zip [0..] ys
         ]

    lxs = length xs
    lys = length ys

main :: IO()
main = do
  s1 <- getLine
  s2 <- getLine
  print $ lcs s1 s2
