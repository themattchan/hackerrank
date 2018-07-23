{-# LANGUAGE TupleSections #-}
import Data.Semigroup (Max(..),Min(..))
import Control.Monad
import Control.Arrow ((&&&))
import Debug.Trace
import qualified Data.Graph as Graph

data Tree = T Int Int [Tree] deriving (Show, Eq)

main :: IO()
main = do
  n <- read <$> getLine
  colours <- map read . words <$> getLine
  edges <- replicateM (n-1) $ do
    [i,j]<- map read . words <$> getLine
    return (i,j)

  let colour n = if colours !! (n-1) == 0
                 then 1 -- white is 1
                 else -1 -- black is -1

  print $ solve colour $ buildTree n edges

buildTree :: Int -> [(Int,Int)] -> Graph.Tree Int
buildTree m = head . Graph.components . Graph.buildG (1,m)

maxStrange :: (Int -> Int) -> Graph.Tree Int -> [Int]
maxStrange colour (Graph.Node x []) = [0, colour x] -- pick or don't pick
maxStrange colour (Graph.Node x ts) =
  let (Max a, Min b) = foldMap ((Max &&& Min) . (+ colour x) . sum)
                       . sequence
                       . map (maxStrange colour)
                       $ ts
  in [0,a,b]

solve :: (Int -> Int) -> Graph.Tree Int -> Int
solve colour = maximum . map abs . maxStrange colour
