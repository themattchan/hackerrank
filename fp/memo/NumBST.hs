-- Catalan Numbers
import Control.Monad

-- https://en.wikipedia.org/wiki/Catalan_number
catalan :: Integer -> Integer
catalan n = ((2*n) `choose` n) `div` (n+1) `mod` bigNumber
  where
    choose n 0 = 1
    choose 0 k = 0
    choose n k = choose (n-1) (k-1) * n `div` k

    bigNumber = (10^8 +7)


main :: IO ()
main = readLn >>= flip replicateM_
        (readLn >>= print . catalan)
