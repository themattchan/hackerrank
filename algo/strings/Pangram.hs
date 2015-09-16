module Pangram where
import Data.Char
import Data.List
import Text.Printf

pangram :: String -> Bool
pangram  = (==26) . length . nub . filter isAsciiLower . map toLower

main = do
    s <- getLine
    let b = pangram s
    printf "%spangram\n" (if b then "" else "not ")
