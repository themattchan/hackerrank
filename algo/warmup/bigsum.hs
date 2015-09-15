import Control.Applicative

main :: IO ()
main = do
  getLine
  xs <- getLine <**> pure (map read . words)
  print (sum xs)
