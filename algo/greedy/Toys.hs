module Toys where
import Data.List
import Control.Monad

maxToys _ [] = 0
maxToys 0 _ = 0
maxToys k (x:xs)
  | k < x = 0
  | otherwise = 1 + maxToys (k - x) xs

main :: IO ()
main = do
  [n,k] <- liftM (map read . words) getLine
  xs <- liftM (map read . words) getLine
  print (maxToys k (sort xs))
