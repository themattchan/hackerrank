{-# LANGUAGE BangPatterns #-}
import Data.Bits
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

-- remember from game theory that xor of [1..n] is given by
-- n mod 4 = 0 -> n
-- n mod 4 = 1 -> 1
-- n mod 4 = 2 -> n+1
-- n mod 4 = 3 -> 0
xorTo :: Integer -> Integer
xorTo n = case n `mod` 4 of
            0 -> n
            1 -> 1
            2 -> n+1
            3 -> 0
            _ -> error "impossible"

isOdd n = n `mod` 2 == 1
isEven n = n `mod` 2 == 0

makeEven n | isEven n = n
           | otherwise = n-1

-- https://math.stackexchange.com/questions/1558404/finding-xor-of-all-even-numbers-from-n-to-m
xorEvensTo :: Integer -> Integer
xorEvensTo n = case (makeEven n) `mod` 8 of
            0 -> n
            2 -> 2
            4 -> n+2
            6 -> 0
            _ -> error "impossible"

xorEvensRange lo hi = xorEvensTo hi `xor` xorEvensTo (lo-1)

xorRange :: Integer -> Integer -> Integer
xorRange lo hi = belows `xor` bounds
  where
    belows | (hi-lo+1) `mod` 2 == 1 = xorTo (lo-1)
           | otherwise = 0
    bounds | isEven hi = xorEvensRange lo hi `xor` (if isOdd lo then lo else 0)
           | otherwise = xorEvensRange lo hi `xor` ((hi-lo+1) `mod` 2)

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    [lo,hi] <- map read . words <$> getLine
    print $ xorRange lo hi
