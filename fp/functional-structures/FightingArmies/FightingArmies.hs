{-# LANGUAGE BangPatterns, TypeApplications, LambdaCase, ScopedTypeVariables,Strict #-}
import qualified Data.Array.MArray as A
import qualified Data.Array.IO as A

import qualified Data.ByteString.Char8 as B8
import qualified Data.IntMap.Strict as IM
import Data.Ord
import Data.List (unfoldr)
import Data.Char
class Heap h where
  empty :: Ord a => h a
  isEmpty :: Ord a => h a -> Bool
  insert :: Ord a => a -> h a -> h a
  merge :: Ord a => h a -> h a -> h a
  findMin :: Ord a => h a -> Maybe a
  deleteMin :: Ord a => h a -> Maybe (a, h a)


-- Lazy Binomial Heap (Okasaki 6.4.1)
data T a = N !Int a [T a]
newtype LazyBinomialHeap a = LBH [T a]

rank :: T a -> Int
rank (N r _ _) = r

root (N _ r _) = r

link t1@(N r x1 c1) t2@(N _ x2 c2)
  | x1 <= x2 = N (r+1) x1 (t2:c1)
  | otherwise = N (r+1) x2 (t1:c2)

insTree t [] = [t]
insTree t (t':ts')
  | rank t < rank t' = (t:ts')
  | otherwise = insTree (link t t') ts'

mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg (t1:ts1) (t2:ts2)
  | rank t1 < rank t2 = t1 : mrg ts1 (t2:ts2)
  | rank t2 < rank t1 = t2 : mrg (t1:ts1) ts2
  | otherwise         = insTree (link t1 t2) (mrg ts1 ts2)

insertLBH x ts = N 0 x [] : ts

removeMinTree :: Ord a => [T a] -> Maybe (T a, [T a])
removeMinTree [] = Nothing
removeMinTree [t] = Just (t, [])
removeMinTree (t:ts) = do
  (t', ts') <- removeMinTree ts
  pure $ if root t <= root t' then (t, ts) else (t', t:ts')

findMinLBH :: Ord a => [T a] -> Maybe a
findMinLBH = fmap (root . fst) . removeMinTree

deleteMinLBH :: Ord a => [T a] -> Maybe (a, [T a])
deleteMinLBH ts = do
  (N _ x ts1, ts2) <- removeMinTree ts
  pure $ (x, mrg (reverse ts1) ts2)

instance Heap LazyBinomialHeap where
  empty = LBH []
  isEmpty (LBH h) = null h
  merge (LBH x) (LBH y) = LBH (mrg x y)
  insert x (LBH h) = LBH (insertLBH x h)
  findMin (LBH h) = findMinLBH h
  deleteMin (LBH h) = fmap LBH <$> deleteMinLBH h


-- Bootstrapped Heap (Okasaki 10.2.2)
-- findMin: O(1) worst case
-- insert, merge: O(1) amortised
-- deleteMin: O(log n) amortised
data BootstrapHeap a = E | H a (LazyBinomialHeap (BootstrapHeap a))

instance Eq a => Eq (BootstrapHeap a) where
  H a _ == H b _= a==b
instance Ord a => Ord (BootstrapHeap a) where
  H a _ `compare` H b _= a `compare` b

instance Heap BootstrapHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x h = merge (H x empty) h

  merge E h = h
  merge h E = h
  merge h1@(H x p1) h2@(H y p2)
    | x <= y    = H x (insert h2 p1)
    | otherwise = H y (insert h1 p2)

  findMin E = Nothing
  findMin (H x p) = Just x

  deleteMin E = Nothing
  deleteMin (H x p)
    | isEmpty p = Just (x, E)
    | otherwise = do
        (H y p1, p2) <- deleteMin p
        pure (x, H y (merge p1 p2))


--------------------------------------------------------------------------------


{-
main = do
  [n,q]<- map (read @Int) . words <$> getLine
  arr :: A.IOArray Int (BootstrapHeap (Down Int)) <- A.newArray (1,n) empty

  let go !q
        | q == 0 = return ()
        | otherwise =
        (map (read @Int) . words <$> getLine) >>=
        \case
          -- print max in i
          [1, i] -> do
            x <- A.readArray arr i
            maybe (pure ()) (\(Down x) -> print x) $
              findMin x

            go (q-1)

          -- remove max from i
          [2, i] -> do
            x <- A.readArray arr i
            case deleteMin x of
              Nothing -> pure ()
              Just (_, h) ->
                A.writeArray arr i h
            go (q-1)

          -- add c to i
          [3, i, c] -> do
            x <- A.readArray arr i
            A.writeArray arr i (insert (Down c) x)
            go (q-1)

          -- merge i and j
          [4, i, j] -> do
            x <- A.readArray arr i
            y <- A.readArray arr j
            A.writeArray arr i (merge x y)
            A.writeArray arr j empty
            go (q-1)

  go q
-}

main = do
  bs <- B8.getContents
--  print $ unfoldr (fmap (fmap (B8.dropWhile isSpace)). B8.readInt) bs
--  [n,q]<- map (read @Int) . words <$> getLine
  let (n:q:ns) = unfoldr (fmap (fmap (B8.dropWhile isSpace)). B8.readInt) bs

  let go !n ns x | n == 0 = return ()
              | otherwise =
        case ns of
          -- print max in i
          (1 : i : ns') -> do
            maybe (pure ()) (\(Down x) -> print x) $
              findMin (IM.findWithDefault empty i x)

            go (n-1) ns' x

          -- remove max from i
          (2 : i : ns') ->
            let f Nothing = Nothing
                f (Just h) = snd <$> deleteMin h
            in go (n-1) ns' (IM.alter f i x)

          -- add c to i
          (3 : i : c : ns') ->
            let f Nothing = Just (insert (Down c) empty)
                f (Just h) = Just (insert (Down c) h)
            in go (n-1) ns' (IM.alter f i x)

          -- merge i and j
          (4 : i : j : ns')->
            let f Nothing = IM.lookup j x -- nothing at i
                f (Just h) = Just (merge (IM.findWithDefault empty j x) h)
            in go (n-1) ns' (IM.delete j (IM.alter f i x))

  go q ns (mempty :: IM.IntMap (BootstrapHeap (Down Int)))
