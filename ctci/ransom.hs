import qualified Data.Map as M
import Data.Bool
import Data.Monoid
import Data.Profunctor

main =
  getLine *>
  (ransom <$> getLine <*> getLine >>= putStrLn . bool "Yes" "No")

ransom xs ys = getAny . foldMap (Any . (< Sum 0))
             $ M.unionWith mappend
               (comb xs) (fmap negate (comb ys))
  where
    tally = flip M.singleton (Sum 1)
    comb = foldr (M.unionWith mappend . tally) mempty . words
