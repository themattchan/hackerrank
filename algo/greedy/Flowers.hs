module Flowers where
import Data.List
import Control.Monad

flowers

main :: IO ()
main = do
  [n,k] <- liftM (map read . words) getLine
  xs <- liftM (map read . words) getLine
  print (flowers k (sort xs))
