{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as A
import Control.Monad
import Data.Foldable
import Data.List

main :: IO ()
main = do
  let readLn = map (read @Int) . words <$> getLine
  [m,n,r] <- readLn
  matrix <- replicateM m readLn
  let matrixA :: A.UArray Int Int = A.listArray (1, m*n) (concat matrix)
--  putStrLn ""
  traverse_ (putStrLn . unwords . map (show . (matrixA A.!))) (doIt m n r)

-- Naive approach: too slow for input09
-- Split into rings, rotate, then recombine

data Ring a = Ring Int {-m: number of ROWS-} Int {-n: number of COLS-} [a]

rings :: Int -> Int -> [[a]] -> [Ring a]
rings m n xxs
  | m-2>0 && n-2>0 = Ring m n r : rings (m-2) (n-2) xxs'
  | otherwise = [Ring m n (base xxs)]
  where
    middle = init . tail

    go x = head x ++ map last mx ++ reverse (last x) ++ reverse (map head mx)
      where mx = middle x

    r = go xxs
    xxs' = map middle (middle xxs)

    base [xs] = xs
    base [x,y] = x ++ reverse y
    base xxs = go xxs

unrings :: [Ring a] -> [[a]]
unrings = stitch . map cutup
  where
    cutup :: Ring a -> ([a],[a],[a],[a])
    cutup (Ring m n xs0) = (ts,rs,reverse bs,reverse ls)
      where
        (ts,xs1) = splitAt n xs0
        (rs,xs2) = splitAt (m-2) xs1
        (bs,ls) = splitAt n xs2

    stitch :: [([a],[a],[a],[a])] -> [[a]]
    stitch [(ts,rs,bs,ls)] = ts : zipWith (\l r -> [l,r]) ls rs ++[bs]
    stitch ((ts,rs,bs,ls):xs) = ts : zipWith3 (\l r x -> l:x++[r]) ls rs (stitch xs) ++ [bs]

rotatel :: Int -> Ring a -> Ring a
rotatel r (Ring m n xs) = Ring m n (y++x)
  where
    (x,y) = splitAt r' xs
    r' = r `mod` siz
    siz = 2*(n+(m-2))

doItSlow :: Int -> Int -> Int -> [[a]] -> [[a]]
doItSlow m n r = unrings . map (rotatel r) . rings m n


--------------------------------------------------------------------------------

-- Fast approach
-- Read into array, calculate rotated indices, then print things out by lookup

doIt :: Int -> Int -> Int -> [[Int]]
doIt m n r = doItSlow m n r [[ col + (row*n) | col <- [1..n] ]  | row <- [0..m-1]]


--translateI :: Int -> Int -> Int -> Int -> Int
--translateI m n r i =
