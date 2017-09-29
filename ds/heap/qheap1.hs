import Control.Monad
import Data.List
import Data.Foldable
import qualified Data.Set as S

do1 :: S.Set Int -> [Int] -> (S.Set Int, IO ())
do1 s l = case l of
  [1, v] -> (S.insert v s, return ())
  [2, v] -> (S.delete v s, return ())
  [3]    -> (s, print $ S.findMin s)

run :: [[Int]] -> IO ()
run = fold . snd . mapAccumL do1 S.empty

main :: IO ()
main = do
  n <- readLn
  input <- replicateM n $ map read . words <$> getLine
  run input
