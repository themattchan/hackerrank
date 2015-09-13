module MaximumSubarray where
import Control.Applicative
import Control.Monad
import Text.Printf

-- Kadane's algorithm, with some extra sauce
kadane :: [Integer] -> (Integer, Integer)
kadane xs = let ans@(mf,ps) = go' xs in
  if all (< 0) xs then (mf, maximum xs) else ans
  where
    go' (x:xs)         = go x x (max 0 x) xs
    go me mf ps []     = (mf, ps)
    go me mf ps (x:xs) = let me' = max x (me + x)
                             mf' = max me' mf
                             ps' = if x > 0 then ps + x else ps
                         in go me' mf' ps' xs


readArray :: IO [Integer]
readArray = do
  getLine
  x <- getLine
  return . map read . words $ x

readInt :: IO Int
readInt = readLn

main :: IO ()
main = do
  n <- readInt
  forM_ [1..n] $ \_ -> do
    (mf, ps) <- readArray <**> pure kadane
    printf "%d %d\n" mf ps
