import qualified Data.Map as M
import Data.Function (on)
import Data.Foldable
import Data.Monoid

-- size of symmetric difference between two multisets
numdeletions = fmap (getSum . fold) . on (M.unionWith (fmap abs . (-))) comb
  where
    tally = flip M.singleton (Sum 1)
    -- the default monoid instance for Map is fucked up
    comb = foldr (M.unionWith mappend . tally) mempty

main = numdeletions <$> getLine <*> getLine >>= print
