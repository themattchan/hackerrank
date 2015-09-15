module Sansa where
import Control.Monad
import Data.Bits (xor)


{-
SOLUTION:

Notice two properties of xor:

* Associativity: a `xor` (b `xor` c) == (a `xor` b) `xor` c
* Commutativity: a `xor` b == b `xor` a

Together these properties give us freedom in reordering xor operations as we
wish.

Also:
* Identities:
   x `xor` 0         = x
   x `xor` x         = 0
   x `xor` x `xor` x = x
   ...

and in general, 0 when `xor`ed with even # copies of itself, and x when odd.

Now let us analyse how many times an element at index i in a given array will
appear in the set of subarrays
(i.e. # of times E_i appears in (concat (subarrays xs)), for each E_i )

For convenience, assume the elements are numbered according to index, and take
the list [1,2,3,4,5,6,7] as a concrete example.

N = 7 (length of list)

We count with the following table (idx down, subarray len across):

  # occs in subarray of length n
# | 1 2 3 4 5 6 7  SUM
------------------------
1 | 1 1 1 1 1 1 1   7           -- odd
2 | 1 2 2 2 2 2 1   12          -- even
3 | 1 2 3 3 3 2 1   15          -- odd
4 | 1 2 3 4 3 2 1   16          -- even
5 | 1 2 3 3 3 2 1   15          -- odd
6 | 1 2 2 2 2 2 1   12          -- even
7 | 1 1 1 1 1 1 1   7           -- odd

Let's examine the sequence of sums:

idx  1     2     3      4      5      6      7
SUM  7     12    15     16     15     12     7
     7*1   6*2   5*3    4*4    3*5    2*6    1*7
     Ni   (N-1)i (N-2)i (N-3)i (N-4)i (N-5)i (N-6)i

Sequence at i = (N-(i-1))*i    [1-indexed]

For 0-indexed:
    7*(0+1), 6*(1+1)... = (N-i) * (i+1)

So for an element at index i in a list of length N, it is repeated (N-i) * (i+1)
times in the subarrays.

By commutativity, we do not have to do the xors by subarray order, but can do
them in sequential order: that is, for each number at index i, xor it with
itself (N-i) * (i+1) times (using the even/odd property), then xor the whole list.

Finally, fuse the self-xor and list-xor into a fold!

-}


-- xor an integer by itself n times
xorN :: Int -> Integer -> Integer
xorN n num | odd n  = num
           | even n = 0

sublistXor :: [Integer] -> Integer
sublistXor xs =
  let len        = length xs
      reps i     = (len-i) * (i+1)
      go x (i,a) = let a' = a `xor` xorN (reps i) x
                   in (i+1, a')
      acc        = (0, 0) -- index, accum
  in snd $ foldr go acc xs


{-
ADDENDUM:

Notice that the sequence has a odd/even/odd pattern.

Actually, it depends on the length of the list:
  odd length:  odd, even, odd...   (drop even indexed elems)
  even length: even, even, even... (all 0)

Use this property instead of calculating the sum.
-}

sublistXor' :: [Integer] -> Integer
sublistXor' xs
  | even (length xs) = 0
  | otherwise        = foldr xor 0 $ zipWith (*) xs (cycle [1,0])


main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    getLine
    xs <- getLine >>= return . map read . words
    print $ sublistXor' xs
