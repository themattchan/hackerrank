module Quicksort where
import Control.Applicative
import Control.Monad
import Data.List

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
  getLine
  ns <- getLine <**> pure (map read . words)
  quicksort ns
  return ()
