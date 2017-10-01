{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
import Data.Traversable
import qualified Data.Semigroup as S
import Data.Monoid
import Control.Arrow (Kleisli(..))
import qualified Control.Category as Cat
import Control.Monad
import qualified Data.Map as M

-- an endomorphic arrow in some category
newtype EndoCat c a = EndoCat { appCat :: Cat.Category c => c a a }
instance S.Semigroup (EndoCat c a) where
  EndoCat f <> EndoCat g = EndoCat (g Cat.. f)
instance Monoid (EndoCat c a) where
  mempty = EndoCat Cat.id
  mappend = (<>)

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Functor

-- inorder
instance Show a => Show (Tree a) where
  show = unwords . foldMap (pure . show)

instance Foldable Tree where
  foldMap _ Leaf         = mempty
  foldMap f (Node e l r) = foldMap f l <> f e <> foldMap f r

instance Traversable Tree where
  traverse _ Leaf         = pure Leaf
  traverse f (Node e l r) = flip Node <$> traverse f l <*> f e <*> traverse f r

swap :: Int -> Tree a -> Tree a
swap _ Leaf         = Leaf
swap 1 (Node e l r) = Node e r l
swap n (Node e l r) = Node e (swap (n-1) l) (swap (n-1) r)

depth :: Tree a -> Int
depth Leaf = 1
depth (Node _ l r) = 1 + max (depth l) (depth r)

allswaps :: Int -> Int -> [Int]
allswaps d i = takeWhile (<= d) $ map (*i) [1..]

buildtree :: M.Map Int [Int] -> Tree Int
buildtree m = go 1
  where
    go i | i == -1 = Leaf
         | otherwise =
           let Just [l,r] = M.lookup i m in
             Node i (go l) (go r)

do1 :: Show a => Int -> Tree a -> IO (Tree a)
do1 d tree = do
  is <- allswaps d <$> readLn
  liftM2 (>>) print return . appEndo (foldMap (Endo . swap) is) $ tree

main :: IO ()
main = do
  n <- readLn
  tree <- buildtree . mconcat <$> forM [1..n] (\i -> M.singleton i . map read . words <$> getLine)
  let d = depth tree
  t <- readLn
  void $ runKleisli (appCat (S.stimes t (EndoCat (Kleisli (do1 d))))) tree
