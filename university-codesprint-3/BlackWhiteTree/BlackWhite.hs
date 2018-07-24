{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, BangPatterns #-}
import Data.Semigroup (Semigroup(..), Max(..),Min(..), (<>))
import Data.Bifunctor
import Control.Monad
import Data.List
import Control.Arrow ((&&&))
import Debug.Trace
import qualified Data.Graph as Graph
import qualified Data.Array.Unboxed as Array
import Data.Word

--https://www.hackerrank.com/contests/university-codesprint-3/challenges/black-white-tree

main :: IO()
main = do
  n <- read <$> getLine
  colours :: Array.UArray Int Word8 <- Array.array (1,n) . zip [1..] . map read . words <$> getLine
  edges <- replicateM (n-1) $ do [i,j] <- map read . words <$> getLine ; return (i,j)
  let weight i | i == 0 = 1
               | i == 1 = -1
  let colour = weight . (colours Array.!)
  let P chosen strangeness = solve colour $ buildTree n edges
  print strangeness
  print (length chosen)
  putStrLn (unwords . map show . sort $ chosen)

buildTree :: Int -> [(Int,Int)] -> Graph.Tree Int
buildTree m = head . Graph.components . Graph.buildG (1,m)

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

-- Solution:
-- At a leaf: can either choose or not
-- At a branch:
--   1. Find the solution for all subtrees. Because we don't know if it'll be black
--   heavy or white heavy, record both. Also return the possibility that it is
--   not chosen.
--   2. (sequence): Calculate the best combination for all possible choices of
--   subtrees. Add the branch node weight.
--   3. If you bubble this up to the top, you will have found the best possible
--   subtree that includes the root.
--   4. HOWEVER this is not enough: at any subtree you can decide to stop, and
--   that tree might be better than the optimal one from the root. Hence we save
--   all subsolutions (using the writer monad).
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
                  . bimap concat sum
                  . unzipP
                  )
        . sequence
        $ ts'
      !thisMax = bimap (x:) (+ colour x) a
      !thisMin = bimap (x:) (+ colour x) b
      w' = (Max (IW thisMax), (Min (IW thisMin)))
      !w'' = w <> w'
  in P w'' [P [] 0 , thisMax, thisMin]

choose :: BestForSub -> P [Int] Int
choose (Max (IW (P chosenMax a)), Min (IW (P chosenMin b)))
  | abs a > abs b = P chosenMax a
  | otherwise = P chosenMin (abs b)

solve :: (Int -> Int) -> Graph.Tree Int -> P [Int] Int
solve colour = choose . fstP . maxStrange colour


--------------------------------------------------------------------------------
-- strict pair
data P a b = P !a !b

fstP :: P a b -> a
fstP (P a _) = a

sndP :: P a b -> b
sndP (P _ b) = b

unzipP :: [P a b] -> P [a] [b]
unzipP ps = P (map fstP ps) (map sndP ps)

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
