{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE TupleSections #-}
import Data.Semigroup (Max(..),Min(..), (<>))
import Data.Bifunctor
import Control.Monad
import Data.List
import Control.Arrow ((&&&))
import Debug.Trace
import qualified Data.Graph as Graph

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

  let (chosen, strangeness) = solve colour $ buildTree n edges
  print strangeness
  print (length chosen)
  putStrLn (unwords . map show . sort $ chosen)

buildTree :: Int -> [(Int,Int)] -> Graph.Tree Int
buildTree m = traceShowId. head . Graph.components . Graph.buildG (1,m)

-- ignore writer
newtype IW b = IW ([Int],b)
instance Ord b => Ord (IW b) where
  compare (IW (a,b)) (IW (x,y)) = compare b y
instance Eq b => Eq (IW b) where
  (==) (IW (a,b)) (IW (x,y)) = b == y
instance Bounded b => Bounded (IW b) where
  maxBound = IW (mempty, maxBound)
  minBound = IW (mempty, minBound)
instance Functor IW where
  fmap f (IW (a,b)) = IW (a,f b)


type BestForSub = (Max (IW Int), Min (IW Int))

choose :: BestForSub -> ([Int], Int)
choose (Max (IW (chosenMax, a)), Min (IW (chosenMin,b)))
  | abs a > abs b = (chosenMax, a)
  | otherwise = (chosenMin, abs b)

-- consider also subtrees only!!!
-- so this is a dynamic programming problem.
maxStrange :: (Int -> Int) -> Graph.Tree Int -> (BestForSub, [([Int], Int)])
maxStrange colour (Graph.Node x []) =
  let this = ([x], colour x)
      w = (Max (IW this), Min (IW this))
  in (w, [([], 0), this])

maxStrange colour (Graph.Node x ts) =
  let (w, ts') = traverse (maxStrange colour) ts
      (Max (IW a), Min (IW b))
        = foldMap ( (Max &&& Min)
                  . IW
                  . bimap concat ((+ colour x) . sum)
                  . unzip
                  )
        . sequence
--        . traceShowId
        $ ts'
      thisMax = first (x:) a
      thisMin = first (x:) b
      w' = (Max (IW thisMax), (Min (IW thisMin)))
  in (w <> w', [([], 0), thisMax, thisMin])

solve :: (Int -> Int) -> Graph.Tree Int -> ([Int], Int)
solve colour = choose . fst . maxStrange colour
