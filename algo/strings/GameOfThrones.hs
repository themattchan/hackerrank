module GOT where
import Data.List

anagramPalindrome :: String -> Bool
anagramPalindrome s = if   even (length s)
                      then length os == 0
                      else length os == 1
  where (_,os) = partition even . map length . group . sort $ s

main :: IO ()
main = do
  xs <- getLine
  putStrLn $ if anagramPalindrome xs then "YES" else "NO"
