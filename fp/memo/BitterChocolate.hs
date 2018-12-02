import Control.Monad
import Data.Bits (xor)
-- Note: this is the game of Chomp!
-- it is equivalent to Nim.
-- https://en.wikipedia.org/wiki/Chomp

-- this variant: 3 x N
-- can player 1 win?
chomp :: [Int] -> Bool
chomp [n1, n2, n3] = (n1 `xor` n2 `xor`n3) == 0

fmt True = "WIN"
fmt False = "LOSE"

main :: IO ()
main = do
  n <- readLn
  replicateM_ n (fmt . chomp . map read . words <$> getLine >>= putStrLn)
