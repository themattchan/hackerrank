import Control.Monad

main :: IO ()
main = do
  n <- readLn
  replicateM_ n (pentagonal <$> readLn >>= print)

pentagonal :: Int -> Int
pentagonal n = (3*(n*n) - n) `div` 2
