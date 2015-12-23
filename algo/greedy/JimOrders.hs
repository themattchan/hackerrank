import Control.Monad
import Data.List



main :: IO ()
main = do
  n <- readLn
  os <- forM [1..n] $ \i -> do
    [t,d] <- liftM (map read . words) getLine
    return (i,t,d)
  return ()
