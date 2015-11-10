module SnakesAndLadders where
import qualified Data.Map as M
import Control.Monad

{-  ALGORITHM:
 - Keep picking maximal dist [6..1], unless there is a ladder closer. Avoid
 - Snakes.
 -}

play :: M.Map Int Int -> Int -> Int
play _ 100 = 0
play sl c   = 1 + play sl (maximum steps)
  where stepTo n = M.findWithDefault n n sl
        possibleSteps = map (+c) [1..6]
        steps = map stepTo possibleSteps

readAssocList :: Int -> IO [(Int, Int)]
readAssocList n = replicateM n readPair
  where readPair = do
          a:b:_ <- liftM (map read . words) getLine
          return (a,b)

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    s <- readLn
    ss <- readAssocList s
    l <- readLn
    ll <- readAssocList l
    let m = M.fromList (ss++ll)
    print (play m 0)
