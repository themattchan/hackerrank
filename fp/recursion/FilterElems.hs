module FilterElems where
import Data.List
import Control.Monad

-- this is gross... why do i have to define this shit
sortFst (a1,a2) (b1,b2) =
  let cf = compare a1 b1 in
  case cf of
    EQ -> compare a2 b2
    _  -> cf


sortSnd (a1,a2) (b1,b2) =
  let cs = compare a2 b2 in
  case cs of
    EQ -> compare a1 b1
    _  -> cs

eqSnd (_, a2) (_, b2) = a2 == b2

minIndex xs@((f,s):_) =
  let projF = map fst xs
      minI  = minimum projF
  in (minI, s)

findReps k xs =
  let xs1 = zip [1..] xs
      xs2 = groupBy eqSnd (sortBy sortSnd xs1)
      xs3 = filter ((>= k) . length) xs2
  in if xs3 == [] then [-1]
     else let
       xs4 = map minIndex xs3
       xs5 = sortBy sortFst xs4
       xs6 = map snd xs5
      in xs6

readIntList :: IO [Int]
readIntList = liftM (map read . words) getLine

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    [_,k] <- readIntList
    as <- readIntList
    let ans = (findReps k as)
    putStrLn $ intercalate " " (map show ans)
