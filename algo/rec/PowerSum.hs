import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  x <- readLn
  n <- readLn
  print $ solve x n

--nroot :: (Integral a, Floating b) => a -> b -> a
nroot n x = fromIntegral $ truncate $  x ** (1 / fromIntegral n)  :: Int

solve :: Int -> Int -> Int
solve x n = go x (nroot n (fromIntegral x))
  where
    go x p
      | x == 0 = 1
      | x < 0 = 0
      | p < 0 = 0
      | otherwise = go x (p-1) + go (x - (p ^ n)) (p-1)
