module Fibonacci where

import Data.List
import Control.Monad

fibonacci :: Int -> Int
fibonacci n = unfoldr (\(a,b) -> Just (a, (b, a+b `mod` m))) (0,1) !! n

m :: Int
m = 10^8 + 7

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    f <- readLn
    print $ fibonacci f `mod` m
