module Fibonacci where

import Data.List
import Control.Monad

-- or just use an accumulator...
fibonacci :: Int -> Integer
fibonacci n = unfoldr (\(a,b) -> Just (a, (b, a+b^2))) (0,1) !! n

m = 10^8 + 7

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    f <- readLn
    print $ fibonacci f `mod` m
