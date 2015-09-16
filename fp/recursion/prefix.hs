module PrefixCompression where

splitWithPrefix :: String -> String -> [String]
splitWithPrefix  = go []
  where
    go ps xs     []     = [reverse ps, xs, []]
    go ps []     ys     = [reverse ps, [], ys]
    go ps (x:xs) (y:ys) = if x == y then go (x:ps) xs ys
                          else [reverse ps, x:xs, y:ys]

printWithLength :: String -> String
printWithLength ss = let l = length ss
                     in (show l) ++ " " ++ ss

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  mapM_ (putStrLn . printWithLength) $
    splitWithPrefix s1 s2
