import Control.Monad
-- Note: this is the game of Chomp!
-- it is equivalent to Nim.
-- https://en.wikipedia.org/wiki/Chomp

-- this variant: 3 x N
-- can player 1 win?
chomp :: [Int] -> Bool
chomp [n1, n2, n3] = undefined

main :: IO ()
main = do
  n <- readLn
  replicateM_ n (chomp . map read . words <$> getLine >>= print)
