module TwoArrays where
import Data.List
import Control.Monad

{-
  Input: two arrays of length n: as, bs
         k, where the zipped sum of some
            permut of as and bs must be >= to

  Idea:
    fix the first array as
    foreach as[i]:
      greedily pick the smallest number bs[j] s.t.
        k-as[i] <= bs[j]

   Functionally:
    1. as' =  map (k-i) over as
    2. sort as' and bs
    3. zipWith (<=) as' bs
    4. ^ must be all TRUE
-}

twoArrays :: Int -> [Int] -> [Int] -> Bool
twoArrays k as bs =
  let as1 = map (flip subtract k) as
      comp1 = zipWith (<=) (sort as1) (sort bs)
  in (and comp1)

readIntList :: IO [Int]
readIntList = liftM (map read . words) getLine

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    [_,k] <- readIntList
    as <- readIntList
    bs <- readIntList
    let ans = (twoArrays k as bs)
    if ans then putStrLn "YES" else putStrLn "NO"
