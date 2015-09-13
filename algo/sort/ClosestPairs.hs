module ClosestPairs where
import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

closest :: [Integer] -> [(Integer,Integer)]
closest = let pairs xs = zipWith (\a b -> ((a,b), abs (b-a))) xs (tail xs)
              cmpSum   = \a b -> snd a `compare` snd b
              eqSum    = \a b -> snd a == snd b
          in map fst . head . groupBy eqSum . sortBy cmpSum . pairs

main :: IO ()
main = do
  getLine
  xs <- getLine <**> pure (map read . words) <**> pure sort
  mapM (uncurry $ printf "%d %d ") $ closest xs
  printf "\n"
