module ToAnagram where
import Data.List

toFreqList :: String -> [(Char, Int)]
toFreqList  s = let fs = group . sort $ s
                    go a [] _          = a
                    go a (c:cs) (f:fs) = if head f == c
                                         then go ((c, length f):a) cs fs
                                         else go ((c,0):a) cs (f:fs)
                    go a (c:cs) fs     = go ((c,0):a) cs fs
                in go [] ['a'..'z'] fs

makeAnagram :: String -> String -> Int
makeAnagram a b = let fa = map snd $ toFreqList a
                      fb = map snd $ toFreqList b
                  in sum . map abs $ zipWith (-) fa fb

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  print $ makeAnagram a b
