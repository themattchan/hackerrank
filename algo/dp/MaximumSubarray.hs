import Control.Monad
import Text.Printf

-- Kadane's algorithm, with some extra sauce
kadane :: [Integer] -> (Integer, Integer)
kadane is = let ans = go' is in
  if all (< 0) is then maximum is <$ ans else ans
  where
    go _  mf ps []     = (mf, ps)
    go me mf ps (x:xs) = let me' = max x (me + x)
                             mf' = max me' mf
                             ps' = if x > 0 then ps + x else ps
                         in go me' mf' ps' xs
    go'         (x:xs) = go x x (max 0 x) xs

readArray :: IO [Integer]
readArray = getLine >> map read . words <$> getLine

main :: IO ()
main = readLn >>= flip replicateM_
       (kadane <$> readArray >>=
        uncurry (printf "%d %d\n"))
