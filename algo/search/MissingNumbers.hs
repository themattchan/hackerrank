import Control.Monad
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Function (on)

main :: IO ()
main = putStrLn . unwords . map show =<< solve <$> readl <*> readl
  where
    readl :: IO [Int]
    readl = getLine *> (map read . words <$> getLine)
    solve = fmap (M.keys . M.filter (/= 0)) . on (M.unionWith (-)) comb
      where
        tally = flip M.singleton (Sum 1)
        -- the default monoid instance for Map is fucked up
        comb = foldr (M.unionWith mappend . tally) mempty
