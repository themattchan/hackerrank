module SequenceOfColors where

import Data.List
import Control.Monad

isValid :: String -> Bool
isValid s = diff && r == g && y == b
  where ((r,g,y,b), diff) = foldl count ((0,0,0,0), True) s
        count ((r,g,y,b), diff) c = case c of
          'R' -> ((r+1,g,y,b), diff && abs (r+1 - g) <= 1)
          'G' -> ((r,g+1,y,b), diff && abs (g+1 - r) <= 1)
          'Y' -> ((r,g,y+1,b), diff && abs (y+1 - b) <= 1)
          'B' -> ((r,g,y,b+1), diff && abs (b+1 - y) <= 1)

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    s <- getLine
    print $ isValid s
