module CountingSort where
import Control.Applicative
import Control.Monad
import Data.List

-- The full counting sort

type IndexedString = (Int, String)

countingSort :: [IndexedString] -> [String]
countingSort xs = let sorted  = sortBy (\a b -> fst a `compare` fst b)  xs
                  in map snd sorted

parseInp1 :: String -> IndexedString
parseInp1 ss = let (i:_) = words ss
              in (read i, "-")

parseInp2 :: String -> IndexedString
parseInp2 ss = let (i:x:_) = words ss
              in (read i, x)

main :: IO ()
main = do
  n  <- getLine <**> pure read
  let n' = n `div` 2
  xs <- replicateM n' getLine <**> pure (map parseInp1)
  ys <- replicateM n' getLine <**> pure (map parseInp2)
  let pp = countingSort (xs++ys)
  putStrLn $ intercalate " " pp
