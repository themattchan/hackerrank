module CountingSort where
import Control.Monad
import Data.List
import qualified Data.Set as S

-- The full counting sort

type IndexedString = (Int, String)

countingSort :: [IndexedString] -> [String]
countingSort xs = let (s1,s2) = splitAt (length xs `div` 2) xs
                      s1'     = S.fromList s1
                      sorted = sortBy (\a b -> fst a `compare` fst b)  xs
                  in map (\x -> if x `S.member` s1' then "-" else snd x) sorted


parseInp :: String -> IndexedString
parseInp ss = let (i:x:_) = words ss
              in (read i, x)

main :: IO ()
main = do
  n  <- getLine >>= return . read
  xs <- replicateM n getLine >>= mapM (return . parseInp)
  let pp = countingSort xs
  putStrLn $ intercalate " " pp
