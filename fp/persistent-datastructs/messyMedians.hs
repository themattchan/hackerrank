--import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import Control.Monad.Cont
import Data.Monoid
import Data.Maybe
import Data.Ord

{-
http://stackoverflow.com/questions/10657503/find-running-median-from-a-stream-of-integers

Step 1: Add next item to one of the heaps

   if next item is smaller than maxHeap root add it to maxHeap,
   else add it to minHeap

Step 2: Balance the heaps (after this step heaps will be either balanced or
   one of them will contain 1 more item)

   if number of elements in one of the heaps is greater than the other by
   more than 1, remove the root element from the one containing more elements and
   add to the other one


Then at any given time you can calculate median like this:

   If the heaps contain equal amount of elements;
     median = (root of maxHeap + root of minHeap)/2
   Else
     median = root of the heap with more elements
-}

type Heap = M.Map Integer Integer
data State = S Int Heap Heap

initState :: Integer -> Integer -> State
initState i j = S 0 (M.singleton (min i j) 1) (M.singleton (max i j) 1)

incOne Nothing  = Just 1
incOne (Just n) = Just (n+1)

decOne Nothing  = Nothing
decOne (Just 1) = Nothing
decOne (Just n) = Just (n-1)

add :: State -> Integer -> State
add (S d maxH minH) n
  | n < sk    = balance dMax maxH' minH
  | otherwise = balance dMin maxH  minH'
  where (sk,sv) = M.findMax maxH
        (maxH', dMax) = (M.alter incOne n maxH, d-1)
        (minH', dMin) = (M.alter incOne n minH, d+1)

        balance d maxH minH
          | d < -1    = undefined
          | d > 1     = undefined
          | otherwise = S d maxH minH

median :: State -> Double
median (S d maxH minH)
  | d == 0 = let (sk,sv) = M.findMax maxH
                 (bk,bv) = M.findMin minH
             in fromIntegral (sk + bk) / 2
  | d < 0  = fromIntegral $ fst $ M.findMax maxH
  | d > 0  = fromIntegral $ fst $ M.findMin minH

step :: State -> Integer -> (State, Double)
step s i = (s', median s')
  where s' = add s i
