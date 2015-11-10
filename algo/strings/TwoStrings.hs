module TwoStrings where
import Data.List
import Control.Monad

-- longest common substring by dynamic programming
-- https://en.wikipedia.org/wiki/Longest_common_substring_problem

prefixes [] = [[]]
prefixes xs = [xs] ++ prefixes (init xs)

lcSuff xs ys = lcSuff' (reverse xs) (reverse ys)
  where lcSuff' (x:xs) (y:ys)
          | x==y = 1 + lcSuff' xs ys
          | otherwise = 0
        lcSuff' _ _ = 0

lcSubstr xs ys = maximum [lcSuff x y | x <- prefixes xs, y <- prefixes ys]

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    s1 <- getLine
    s2 <- getLine
    let subs = lcSubstr s1 s2
    if subs > 0
      then putStrLn "YES"
      else putStrLn "NO"
