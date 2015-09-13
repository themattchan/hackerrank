import Data.List
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    s <- getLine
    let s' = map head (group s)
    print (length s - length s')
