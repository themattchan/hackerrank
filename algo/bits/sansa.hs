import Data.Bits
import Data.List
import Control.Monad

{- TOO SLOW -}
sublistXor xs = let subs = concatMap tails . inits $ xs in
                 foldl' (\a x -> a `xor` (xors x)) 0 subs
  --foldl1' xor $ map xors $ subs xs
  where
    xors xs@(x:y:ys) = foldl1' xor xs
    xors [x]         = x
    xors []          = 0 -- identity

toInt :: String -> Int
toInt = read

main :: IO ()
main = do
  gos <- readLn
  replicateM_ gos $ do
    getLine
    ns <- getLine
    print $ sublistXor (map toInt $ words ns)
