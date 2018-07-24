{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE TupleSections, BangPatterns #-}
import Data.Semigroup (Semigroup(..), Max(..),Min(..), (<>))
import Data.Bifunctor
import Control.Monad
import Data.List
import Control.Arrow ((&&&))
import Debug.Trace
import qualified Data.Graph as Graph

--https://www.hackerrank.com/contests/university-codesprint-3/challenges/black-white-tree
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

  let P chosen strangeness = solve colour $ buildTree n edges
  print strangeness
  print (length chosen)
  putStrLn (unwords . map show . sort $ chosen)

buildTree :: Int -> [(Int,Int)] -> Graph.Tree Int
buildTree m = traceShowId. head . Graph.components . Graph.buildG (1,m)


--strict pair
data P a b = P !a !b
fstP (P a _) = a
sndP (P _ b) = b
instance Functor (P a) where
  fmap f (P a b) = P a (f b)
instance Bifunctor P where
  bimap f g (P a b) = P (f a) (g b)
instance Monoid a => Applicative (P a) where
  pure = P mempty
  P a f <*> P b x = P (a`mappend`b) (f x)
instance Monoid a => Monad (P a) where
  return = P mempty
  P a x >>= f = let P b y = f x in P (a`mappend`b) y
instance Foldable (P a) where
  foldMap f (P a b) = f b
instance Traversable (P a) where
  traverse f (P a b) = P a <$> f b
instance (Semigroup a, Semigroup b) => Semigroup (P a b) where
  (<>) (P a b) (P x y) = P (a<>x) (b<>y)
instance (Monoid a, Monoid b) => Monoid (P a b) where
  mempty = P mempty mempty
  mappend (P a b) (P x y) = P (a`mappend`x) (b`mappend`y)

unzipP :: [P a b] -> P [a] [b]
unzipP ps = P (map fstP ps) (map sndP ps)

-- ignore writer
newtype IW = IW (P [Int] Int)
instance Ord IW where
  compare (IW (P a b)) (IW (P x y)) = compare b y
instance Eq IW where
  (==) (IW (P a b)) (IW (P x y)) = b == y
instance Bounded IW where
  maxBound = IW (P mempty maxBound)
  minBound = IW (P mempty minBound)

type BestForSub = (Max IW, Min IW)

choose :: BestForSub -> P [Int] Int
choose (Max (IW (P chosenMax a)), Min (IW (P chosenMin b)))
  | abs a > abs b = P chosenMax a
  | otherwise = P chosenMin (abs b)

-- consider also subtrees only!!!
-- so this is a dynamic programming problem.
maxStrange :: (Int -> Int) -> Graph.Tree Int -> P BestForSub [P [Int] Int]
maxStrange colour (Graph.Node x []) =
  let !this = P [x] (colour x)
      !w = (Max (IW this), Min (IW this))
  in P w [P [] 0, this]

maxStrange colour (Graph.Node x ts) =
  let P w ts' = traverse (maxStrange colour) ts
      (Max (IW a), Min (IW b))
        = foldMap ( (Max &&& Min)
                  . IW
                  . bimap concat ((+ colour x) . sum)
                  . unzipP
                  )
        . sequence
--        . traceShowId
        $ ts'
      !thisMax = first (x:) a
      !thisMin = first (x:) b
      w' = (Max (IW thisMax), (Min (IW thisMin)))
      !w'' = w <> w'
  in P w'' [P [] 0 , thisMax, thisMin]

solve :: (Int -> Int) -> Graph.Tree Int -> P [Int] Int
solve colour = choose . fstP . maxStrange colour
