module Flowers where
import Data.List
import Data.List.Split
import Control.Monad


{-
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (y,ys) = splitAt k xs in
  [y] ++ chunksOf k ys
-}

{-
Input: k people,
       unordered list of costs of n flowers [c1..cn]

   for person j, cost of xth flower is (x+1)*ci

Output: minimum spent on flowers

How many ways to partition n items into k groups s.t. sum of each group is
minimised?

Minimum cost of a group [ci..cj] for person p is
- sort [ci..cj]
- (1*ci + 2*ci+1... (j-i+1)*cj)

Idea: each person buys the most expensive flower for each round (since
      (min)*(max) is minimised). Then recurse.

Algorithm:
  1. reverse sort
  2. group by k
  3. sum (sum (map *index))
-}

flowers :: Int -> [Int] -> Int
flowers k =
  let revSort    = reverse . sort
      groupK     = chunksOf k
      sumRound i = sum . map (*i)
      rounds     = zipWith sumRound [1..]
  in (sum . rounds . groupK . revSort)


main :: IO ()
main = do
  [_,k]   <- liftM (map read . words) getLine
  fs <- liftM (map read . words) getLine
  print (flowers k fs)
