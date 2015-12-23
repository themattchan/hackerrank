#!/usr/bin/env runhaskell

import Control.Monad
import Data.List

isAscending :: (Ord a) => [a] -> Bool
isAscending xs = and $ zipWith (<=) xs (tail xs)

gridPermutable :: (Ord a) => [[a]] -> Bool
gridPermutable = and . map isAscending . transpose . map sort

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    s <- readLn
    bs <- forM [1..s] (const getLine)
    putStrLn $ if gridPermutable bs then "YES" else "NO"
