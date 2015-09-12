module CountingSort where
import Control.Monad
import Data.List

-- The full counting sort

type IndexedString = (Int, String)

countingSort :: [IndexedString] -> [String]
countingSort xs = let (_,ss) = splitAt (length xs `div` 2) xs
                      sorted = sortOn fst  xs
                  in map snd sorted


parseInp :: String -> IndexedString
parseInp ss = let (i:x:_) = words ss
              in (read i, x)

main :: IO ()
main = do
  n  <- getLine >>= return . read
  xs <- replicateM n getLine >>= mapM (return . parseInp)
  print xs
  return ()
