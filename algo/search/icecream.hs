import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    m <- readLn
    n <- readLn
    xs <- map read . words <$> getLine
    putStrLn . unwords . map show $ icecream m n xs


icecream :: Int -> Int -> [Int] -> [Int]
icecream tot n = sort
               . fromMaybe [] . getFirst
               . foldMap (uncurry find) -- do binsearch on each tail
               . zip [n,n-1..]          -- annot each tail w/ length
               . init . tails           -- tails nonempty
               . sortOn snd             -- sort by cost
               . zip [1..]              -- index the costs
  where
    find :: Int -> [(Int,Int)] -> First [Int]
    find len ((i,c):ics) = (i:) <$> binsearch (len-1) (tot-c) ics

    binsearch :: Int -> Int -> [(Int,Int)] -> First [Int]
    binsearch _ _ [] = mempty
    binsearch n t xs
      | x == t    = First (Just [i])
      | x < t     = binsearch rn t r
      | otherwise = binsearch ln t l
      where
        n' = n `div` 2
        (ln,rn) | n `mod` 2 == 0 = (n',n'-1)
                | otherwise      = (n', n')
        (l,(i,x):r) = splitAt n' xs
