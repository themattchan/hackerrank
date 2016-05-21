import Control.Monad
import Data.List (foldl')

isBal :: String -> Bool
isBal = null . foldl' match []
  where match ('(':s) ')' = s
        match ('[':s) ']' = s
        match ('{':s) '}' = s
        match s x         = x:s

main :: IO ()
main = do
  getLine
  cts <- getContents
  let cases = lines cts
  forM_ cases $ \x -> putStrLn $ if isBal x then "YES" else "NO"
