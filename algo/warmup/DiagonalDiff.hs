module DiagonalDiff where
import Control.Monad

type Matrix = [[Int]]

diags :: Matrix -> ([Int], [Int])
diags mtx = (ld, rd)
  where
    size    = length mtx
    ls      = [0..size-1]
    takefs  = map (flip (!!))
    ld      = zipWith ($) (takefs ls) mtx
    rd      = zipWith ($) (takefs . reverse $ ls) mtx

readMatrix :: Int -> IO Matrix
readMatrix n = replicateM n $
  do xs <- getLine
     let row = map read . words $ xs
     return row

main :: IO ()
main = do
  n <- readLn
  mtx <- readMatrix n
  let (ld,rd) = diags mtx
  print . abs $ sum ld - sum rd
