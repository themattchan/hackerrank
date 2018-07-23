{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -O2 #-}

import qualified Data.Array as A
import qualified Data.Array.MArray as AM
import qualified Data.Array.ST as AM
import qualified Data.Array.IO as AM
import qualified Control.Monad.ST as ST
import Control.Monad

--               1                                   , K = 0
-- count(N, K) = 1                                   , K = N
--               count(N-1, K-1) + count(N-1, K),    , 0 < K < N


m :: Int
m = 10^8 + 7

-- 0 <= K <= N
ncr :: Int -> Int -> Int
ncr n k = a A.! (n,k)
  where
    a = A.array ((0,0), (n,k)) $
        [ ((i,j),
           if j==0 || j==i
           then 1
           else ((a A.! (i-1,j-1) `mod` m) + (a A.! (i-1,j)) `mod` m) `mod` m)
        | i <- [0..n], j <- [0..min i k]
        ]

{-
int binomialCoeff(int n, int k)
{
    int C[k+1];
    memset(C, 0, sizeof(C));

    C[0] = 1;  // nC0 is 1

    for (int i = 1; i <= n; i++)
    {
        // Compute next row of pascal triangle using
        // the previous row
        for (int j = min(i, k); j > 0; j--)
            C[j] = C[j] + C[j-1];
    }
    return C[k];
}

-}

-- 0 <= K <= N
ncr' :: Int -> Int -> Int
ncr' n k = ST.runST $ do
      arr <- AM.newArray (0,k+1) 0 :: ST.ST s (AM.STUArray s Int Int)
      AM.writeArray arr 0 1
      forM_ [1..n] $ \i -> do
        forM_ (reverse [1..min i k]) $ \j -> do
          x <- AM.readArray arr (j-1)
          y <- AM.readArray arr j
          let !p = (x + y) `mod` m
          AM.writeArray arr j p
      AM.readArray arr k

ncrIO :: Int -> Int -> IO Int
ncrIO n k = do
      arr <- AM.newArray (0,k+1) 0 :: IO (AM.IOUArray Int Int)
      AM.writeArray arr 0 1
      forM_ [1..n] $ \i -> do
        forM_ (reverse [1..min i k]) $ \j -> do
          x <- AM.readArray arr (j-1)
          y <- AM.readArray arr j
          let !p = (x + y) `mod` m
          AM.writeArray arr j p
      AM.readArray arr k

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    [n,k] <- map read . words <$> getLine
    print =<< ncrIO n k
