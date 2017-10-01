import Control.Monad
import Data.Bool

main :: IO ()
main = do
  n <- readLn
  replicateM_ n (getLine *> (map read . words <$> getLine) >>= (putStrLn . bool "NO" "YES" . solve) )

solve :: [Int] -> Bool
solve [] = True
solve (x : xs) = let (less, more) = span (< x) xs
                 in  all (> x) more && solve less && solve more
