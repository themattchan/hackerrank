{-# LANGUAGE RankNTypes, ScopedTypeVariables,TupleSections #-}
{-# OPTIONS_GHC -O2 #-}

import qualified Data.Array as A
import qualified Data.Array.MArray as AM
import qualified Data.Array.ST as AM
import qualified Data.Array.IO as AM
import qualified Control.Monad.ST as ST
import Control.Monad
import Data.Monoid
import Data.Semigroup (Max(..))
import qualified Control.Monad.Cont as Cont

import qualified Data.Map as M
--               1                                   , K = 0
-- count(N, K) = 1                                   , K = N
--               count(N-1, K-1) + count(N-1, K),    , 0 < K < N


m :: Int
m = 10^8 + 7

-- ncr n k = (n!) / (k!(n-k)!)
myncr :: Int -> Int -> Integer
myncr n k = (f n `div` (f k * f (n-k))) `mod` m
  where
    f x = facs !! x

    m :: Integer
    m = 10^8 + 7

    facs :: [Integer]
    facs = go 1 1 where go p n = p : go (p*n) (n+1)

{-
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

-- process all queries at once.
ncrIOs :: M.Map (Int,Int) a -> IO (M.Map (Int,Int) Int)
ncrIOs nks = do
  let (maxN, _) = fst $ M.findMax nks
  let (Max maxK) = M.foldMapWithKey (\(n,k) _ -> pure k) nks

  arr <- AM.newArray (0,maxK+1) 0 :: IO (AM.IOUArray Int Int)
  AM.writeArray arr 0 1

-- this should be made into some sort of coroutine
-- so we will only compute until the next needed (n,k)
-- and unfold it during traversal of nks
-- but i'm too lazy to figure that out
  let
    fill :: M.Map (Int,Int) a -> IO (M.Map (Int,Int) Int)
    fill needed = do
      foldM (\ret0 i ->
                foldM (\ret1 j -> do
                          x <- AM.readArray arr (j-1)
                          y <- AM.readArray arr j
                          let !p = (x + y) `mod` m
                          AM.writeArray arr j p
                          if (i,j) `M.member` needed
                            then return (M.insert (i,j) p ret1)
                            else return ret1
                      )
                      ret0
                      (reverse [1..min i maxK])
            )
            M.empty
            [1..maxN]

  fill nks
-}

main :: IO ()
main = do
  t <- readLn
  nks <- replicateM t $ do
    [n,k] <- map read . words <$> getLine
    print $ myncr n k
