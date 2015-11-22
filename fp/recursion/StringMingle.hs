module StringMingle where

import Control.Monad

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  let mingled = concatMap (\(a,b) -> [a,b]) $ zip s1 s2
  putStrLn mingled
