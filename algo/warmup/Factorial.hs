main :: IO ()
main = do
  n <- readLn
  print $ product [1..n]
