import Data.Bits

maxXOR :: Int -> Int -> Int
maxXOR l r = maximum [x `xor` y | x <- [l..r], y <- [l..r]]

main :: IO ()
main = do
  l <- readLn
  r <- readLn
  print $ maxXOR l r
