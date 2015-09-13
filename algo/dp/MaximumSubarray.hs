module MaximumSubarray where
import Control.Monad
import Text.Printf

-- Kadane's algorithm, with some extra sauce
kadane :: [Integer] -> (Integer, Integer)
kadane = go 0 0 0
  where
    go me mf ps []     = (mf, ps)
    go me mf ps (x:xs) = let me' = max x (me + x)
                             mf' = max me' mf
                             ps' = if x > 0 then ps + x else ps
                         in go me' mf' ps' xs


readArray :: IO [Integer]
readArray = do
  getLine
  x <- getLine
  return . map (\x -> (read x)::Integer) . words $ x

readInt :: IO Int
readInt = readLn

main :: IO ()
main = do
  n <- readInt
  forM [1..n] $ \_ -> do
    (mf, ps) <- readArray >>= return . kadane
    printf "%d %d\n" mf ps
  return ()