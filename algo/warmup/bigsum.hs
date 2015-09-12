import Control.Applicative
import Control.Monad


main :: IO ()
main = do
  getLine
  xs <- getLine <**> pure words >>= return . map (\x -> (read x)::Integer)
  print (sum xs)
