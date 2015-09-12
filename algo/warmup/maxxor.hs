import Data.Bits

maxXor :: Int -> Int -> Int
maxXor l r = foldl1 max [x `xor` y | x<-[l..r], y<-[l..r]]

main :: IO ()
main = do
  l <- readLn
  r <- readLn
  print $ maxXor l r
