module PrefixCompression where

splitWithPrefix :: String -> String -> [String]
splitWithPrefix  = go []
  where
    go as xs     []     = [reverse as, xs, []]
    go as []     ys     = [reverse as, [], ys]
    go as (x:xs) (y:ys) = if x == y then go (x:as) xs ys
                          else [reverse as, x:xs, y:ys]

printWithLength :: String -> String
printWithLength ss = let l = length ss
                     in (show l) ++ " " ++ ss

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  mapM_ (putStrLn . printWithLength) $
    splitWithPrefix s1 s2
