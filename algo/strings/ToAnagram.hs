module ToAnagram where
import Data.List
import Data.Ord

toCounts :: String -> [Int]
toCounts  s = let fs       = group . sort $ s
                  chars    = map head fs
                  freqs    = map length fs
                  missings = ['a'..'z'] \\ chars
              in map snd . sortBy (comparing fst) $
                 zip chars freqs ++ zip missings (repeat 0)


makeAnagram :: String -> String -> Int
makeAnagram a b = let fa = toCounts a
                      fb = toCounts b
                  in sum . map abs $ zipWith (-) fa fb

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  print $ makeAnagram a b
