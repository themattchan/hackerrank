module Gemstones where
import Control.Monad
import Data.List

gemElems :: [String] -> Int
gemElems  = length . foldr1 intersect . map nub

main :: IO ()
main = do
  n  <- readLn
  xs <- replicateM n getLine
  print $ gemElems xs
