module PrefixCompression where

splitWithPrefix :: String -> String -> (String, String, String)
splitWithPrefix  = go []
  where
    go as xs     []     = (reverse as, xs, [])
    go as []     ys     = (reverse as, [], ys)
    go as (x:xs) (y:ys) = if x == y then go (x:as) xs ys
                          else (reverse as, x:xs, y:ys)

printWithLength :: String -> IO ()
printWithLength ss = let l = length ss in
  putStrLn $ (show l) ++ " " ++ ss

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  let (prefix, ss1, ss2) = splitWithPrefix s1 s2
  printWithLength prefix
  printWithLength ss1
  printWithLength ss2
