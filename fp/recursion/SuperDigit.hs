module SuperDigit where

import Control.Monad

super :: Integer -> Integer
super = go . digits
  where go [d] = d
        go ds  = super (sum ds)
        digits = map (read . return) . show

main :: IO ()
main = do
  [n,k] <- liftM (map read . words) getLine
  print $ super ((super n) * k)
