module Quicksort where
import Data.List
import Control.Applicative

displayList :: [Int] -> String
displayList = intercalate " " . map show

-- part 1
qs1 :: [Int] -> [Int]
qs1 (x:xs) = let (sm, gt) = partition (<= x) xs in
  sm ++ [x] ++ gt

main :: IO ()
main = do
  getLine -- # of ns not needed
  ns <- getLine >>= mapM (\x -> pure ((read x)::Int)) . words
  let sorted = qs1 ns
  putStrLn $ displayList sorted
