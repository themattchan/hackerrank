module Quicksort where
import Data.List
import Control.Applicative
import Control.Monad

displayList :: [Int] -> String
displayList = intercalate " " . map show

-- part 1
qs1 :: [Int] -> [Int]
qs1 (x:xs) = let (sm, gt) = partition (<= x) xs in
  sm ++ [x] ++ gt

-- part 2
quicksort :: [Int] -> IO [Int]
quicksort []     = return []
quicksort (x:xs) = let (sm, gt) = partition (<= x) xs in
  do
    ss <- quicksort sm
    sg <- quicksort gt
    let sorted' = ss ++ [x] ++ sg
    when (length sorted' > 1) (putStrLn (displayList sorted'))
    return sorted'

main :: IO ()
main = do
  getLine -- # of ns not needed
  ns <- getLine >>= mapM (\x -> pure ((read x)::Int)) . words
  quicksort ns
  return ()
  --putStrLn $ displayList sorted