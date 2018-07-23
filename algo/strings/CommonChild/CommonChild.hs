{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -O2 #-}
import Debug.Trace
import Control.Monad
import qualified Control.Monad.ST as ST
import qualified Data.Array as A
import qualified Data.Array.MArray as AM
import qualified Data.Array.ST as AM
import qualified Data.Array.IO as AM
import Data.List
import Control.Arrow ((&&&))

--------------------------------------------------------------------------------
-- save whole table (very slow for large input)
lcs xs ys = lcs' (lxs,lys)
  where
    a = A.array ((0,0), (lxs, lys)) go

    lcs' (i,j) | i < 0 || j < 0 = 0
               | otherwise = a A.! (i,j)

    go = [ ((i,j),
            if x == y
            then 1 + lcs' (i-1,j-1)
            else max (lcs' (i-1,j)) (lcs' (i,j-1)))
         | (i,x) <- zip [0..] xs, (j,y) <- zip [0..] ys
         ]

    lxs = length xs -1
    lys = length ys -1

--------------------------------------------------------------------------------

-- save previous row only
lcs2 xs ys = go initRow ys A.! lxs
  where
    initRow = A.array (0, lxs) [(i,0) | (i,_) <- zip [0..] xs]

    get a i | i < 0 = 0
            | otherwise = a A.! i

    go old [] = old
    go old (y:ys') = go new ys'
      where
        new = A.array (0,lxs)
          [ (i, if x == y
                then 1 + get old (i-1)
                else max (get new (i-1)) (get old i))
          | (i,x) <- zip [0..] xs ]

    lxs = length xs -1
    lys = length ys -1

--------------------------------------------------------------------------------

-- save previous row only, using lists (still too slow)
lcs3 xs ys =  go initRow ys
  where
    initRow = map (const 0) xs

    get a i | i < 0 = 0
            | otherwise = a !! i

    go old [] = old
    go old (y:ys') = -- trace ("NEW: " ++ show new) $
                     go new ys'
      where
        new =
          [ if x == y
            then 1 + get old (i-1)
            else max (get new (i-1)) (get old i)
          | (i,x) <- zip [0..] xs ]

    lxs = length xs -1
    lys = length ys -1

--------------------------------------------------------------------------------

-- turn the production of the new list into a fold of the old list, since
-- we only need a square...
-- on input 05: ./CommonChild  10.89s user 1.43s system 97% cpu 12.674 total
lcs4 xs ys = go initRow ys
  where
    initRow = map (const 0) xs

    go old [] = old
    go old (y:ys') = -- trace ("NEW: " ++ show new) $
                     go new ys'
      where
        gen _ [] = []
        gen p (((i1j1, i1j),x) : rest)
          = let t = if x == y then 1 + i1j1 else max i1j p
            in t : gen t rest

        new = gen 0
            . zip (zip (0 : old) old)
            $ xs

--------------------------------------------------------------------------------

-- use recursion combinators
-- think of this as producing "sliding L's"
-- TODO make this more pointfree
--
-- on input05: ./CommonChild  30.97s user 5.76s system 91% cpu 39.954 total
-- Note: the state monad that mapAccumL uses is really slow
lcs41 :: String -> String -> Int
lcs41 xs = fst . foldl' go (0, map (const 0) xs)
  where
    go (_, old) y
      = mapAccumL (\p (i1j1, i1j, x) -> let t = if x == y then 1 + i1j1 else max i1j p in (t,t)) 0
      $ zip3 (0 : old) old xs

-- on input05: ./CommonChild  9.52s user 1.47s system 98% cpu 11.206 total
lcs42 :: String -> String -> Int
lcs42 xs = last . foldl' go (map (const 0) xs)
  where
    go old y
      = unfoldr (\(p, oo) -> case oo of
                    [] -> Nothing
                    (i1j1, i1j, x):oo' ->
                      let t = if x == y then 1 + i1j1 else max i1j p
                      in Just (t, (t, oo'))
                )
      . (0, )
      $ zip3 (0 : old) old xs

-- ./CommonChild  8.13s user 0.96s system 97% cpu 9.358 total
lcs43 :: String -> String -> Int
lcs43 xs = last . foldl' go (map (const 0) xs)
  where
    go old y
      = unfoldr (\oo -> case oo of
                    (_,_,[],[]) -> Nothing
                    (p, i1j1:old0', i1j:old', x:xs') ->
                      let t = if x == y then 1 + i1j1 else max i1j p
                      in Just (t, (t, old0', old', xs'))
                    _ -> Nothing
                )
                (0, 0:old, old, xs)


--------------------------------------------------------------------------------
-- save previous row only
-- on input 05: ./CommonChild  0.32s user 0.02s system 88% cpu 0.384 total
lcs5 xs ys = ST.runST $ do
    old <- AM.newArray (0,lxs) 0 :: ST.ST s (AM.STUArray s Int Int)
    new <- AM.newArray (0,lxs) 0 :: ST.ST s (AM.STUArray s Int Int)
    (result, _) <- foldM go (old,new) ys
    AM.readArray result lxs
  where
    go (old,new) y = do
      let go1 p (i,x) = do
            aa <- if x == y
                  then (1+) <$> AM.readArray old (i-1)
                  else  max p <$> AM.readArray old i
            _ <- AM.writeArray new i aa
            pure aa
      _ <- foldM go1 0 (zip [1..] xs)
      pure (new,old)

    lxs = length xs

--------------------------------------------------------------------------------

-- on input 05: ./CommonChild  0.39s user 0.02s system 83% cpu 0.483 total
lcs6 xs ys = do
    old <- AM.newArray (0,lxs) 0 :: IO (AM.IOUArray Int Int)
    new <- AM.newArray (0,lxs) 0 :: IO (AM.IOUArray Int Int)
    (result, _) <- foldM go (old, new) ys
    AM.readArray result lxs
  where
    go (old,new) y = do
      let go1 p (i,x) = do
            aa <- if x == y
                  then (1+) <$> AM.readArray old (i-1)
                  else  max p <$> AM.readArray old i
            _ <- AM.writeArray new i aa
            pure aa
      _ <- foldM go1 0 (zip [1..] xs)
      pure (new,old)

    lxs = length xs

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  print $ lcs5 s1 s2
--  print =<< lcs6 s1 s2
