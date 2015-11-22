module FiboModified where

import Data.List
import Control.Monad

-- or just use an accumulator...
generalizedFibonacci :: (Integer, Integer) -> Int -> Integer
generalizedFibonacci seed n = unfoldr (\(a,b) -> Just (a, (b, a+b^2))) seed !! (n-1)

main :: IO ()
main = do
  [a,b,n] <- liftM words getLine
  let a' = read a :: Integer
      b' = read b :: Integer
      n' = read n :: Int
  print $ generalizedFibonacci (a',b') n'
