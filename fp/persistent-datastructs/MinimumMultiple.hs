import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.List
import Data.Semigroup

data Tree a
  = Leaf a
  | Branch a (Tree a) (Tree a)
  deriving Show

value :: Tree a -> a
value (Leaf x) = x
value (Branch x _ _) = x

instance Semigroup a => Semigroup (Tree a) where
  t1 <> t2 = Branch (value t1 <> value t2) t1 t2

-- An interval [a,b]
newtype Range = Range { getRange :: (Int,Int) }
  deriving (Show, Eq)

-- Assuming no gaps and no overlapping
instance Semigroup Range where
  (Range (lo,_)) <> (Range (_,hi)) =
    Range (lo,hi)

subrangeOf, highest, lowest :: Range -> Range -> Bool
(Range (slo,shi)) `subrangeOf` (Range (lo,hi)) =
  slo >= lo && shi <= hi

(Range (_,shi)) `highest` (Range (_,hi)) = shi == hi
(Range (slo,_)) `lowest` (Range (lo,_))  = slo == lo

makeRangeTree :: [Int] -> Maybe (Tree (Range, Min Int))
makeRangeTree = listToMaybe . build . map singleton . zip [1..]
  where
    singleton (idx,val) = Leaf $ (Range (idx,idx), Min val)

    build (x:y:zs) = build $ x <> y : build zs
    build x        = x

type Index = Int

update :: Applicative m
       => Index
       -> m (a -> a)
       -> Tree (Range, m a)
       -> Tree (Range, m a)
update idx f tree = undefined

query :: Monoid a => Range -> Tree (Range, a) -> a
query rng@(Range (lo,hi)) t = case t of
  Leaf (_,x) -> x
  Branch (br,x) l r
    | 
    
  

data Action = Q Range | U Index (Min (Int -> Int))

parseAction :: String -> Maybe Action
parseAction s = case words s of
  ("Q" : lo  : hi   : _) -> Just $ Q (Range (read lo, read hi))
  ("U" : idx : mult : _) -> Just $ U (read idx) (pure (* (read mult)))
  _                      -> Nothing

bigNumber :: Int
bigNumber = 10^9 + 7

executeAction :: Tree (Range, Min Int)
              -> Action
              -> IO (Tree (Range, Min Int))
executeAction t (U idx val) = return $ update idx val t
executeAction t (Q range)   = do
  print $ getMin (query range t) `mod` bigNumber
  return t
