import Data.Bits (xor)
import Data.List(foldl')
import Control.Monad(replicateM_)
-- A0 = 0
-- An = A(n-1) XOR n
-- so An = XOR 1..n
-- XOR [Li ..Ri] = A(Li) XOR ... XOR A(Ri)
-- so (XOR Li-1) is repeated (Ri-Li+1) times = do it (Ri-Li+1)%2 times
-- Now consider Li .. Ri
-- Ri is done once, Ri-1 done twice, Ri-2 done thrice...
-- Counting from Ri, drop every second number.


xorRange :: Integer -> Integer -> Integer
xorRange lo hi = belows `xor` bounds
  where
    belows | (hi-lo+1) `mod` 2 == 1 = foldl' xor 0 [1..lo-1]
           | otherwise = 0
    bounds = foldl' xor 0 [hi, hi-2 .. lo]

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    [lo,hi] <- map read . words <$> getLine
    print $ xorRange lo hi
