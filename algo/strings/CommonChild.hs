import Control.Monad
import qualified Data.Array as A

lcs xs ys = a A.! (lxs,lys)
  where
    a = A.array ((0,0), (lxs, lys)) go

    getA (i,j) | i < 0 || j < 0 = 0
               | otherwise = a A.! (i,j)

    go = [ ((i,j),
            if x == y
            then 1 + getA (i-1,j-1)
            else max (getA (i-1,j)) (getA (i,j-1)))
         | (i,x) <- zip [0..] xs, (j,y) <- zip [0..] ys
         ]

    lxs = length xs -1
    lys = length ys -1

main :: IO()
main = do
  s1 <- getLine
  s2 <- getLine
  print $ lcs s1 s2
