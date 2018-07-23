import Debug.Trace
import Control.Monad
import qualified Control.Monad.ST as ST
import qualified Data.Array as A
import qualified Data.Array.MArray as AM
import qualified Data.Array.ST as AM
import qualified Data.Array.IO as AM

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
    go old (y:ys') = trace ("NEW: " ++ show new) $  go new ys'
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
lcs4 xs ys = go initRow ys
  where
    initRow = map (const 0) xs

    go old [] = old
    go old (y:ys') = -- trace ("NEW: " ++ show new) $
                go new ys'
      where
        new = reverse
            . snd
            . foldl (\(ij1, prod) ((i1j1, i1j), x) ->
                       let this = if x == y then 1+i1j1 else max i1j ij1
                       in (this, this : prod)
                    )
                    (0, [])
            . zip old'
            $ xs

        old' = zip (0 : old) old

--------------------------------------------------------------------------------
-- save previous row only
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
  print =<< lcs6 s1 s2
