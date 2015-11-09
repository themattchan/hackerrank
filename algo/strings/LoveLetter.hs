module LoveLetter where
import Data.Char
import Control.Monad

half :: String -> (String, String)
half xs = let (as, bs) = splitAt (length xs `div` 2) xs
              bs'      = if odd (length xs) then tail bs else bs
          in (as, reverse bs')

countPal :: (String, String) -> Int
countPal = sum . uncurry (zipWith f)
  where f x y = abs (ord x - ord y)

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    str <- getLine
    print $ (countPal . half) str
