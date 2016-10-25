import Control.Applicative
import Control.Monad
import Text.Printf

-- shoelace formula
-- https://en.wikipedia.org/wiki/Shoelace_formula
polyArea :: [(Int,Int)] -> Double
polyArea verts = (fromIntegral (down - up)) / 2
  where
    rotate (y:ys) = ys ++ [y]
    offsetMult xs ys = sum $ zipWith (*) xs (rotate ys)
    (xs,ys) = unzip verts
    down    = offsetMult xs ys
    up      = offsetMult ys xs

polyPerimeter :: [(Int,Int)] -> Double
polyPerimeter pts@(p:_) = go (pts ++ [p])
  where
    go ((x1,y1):rest@((x2,y2):_)) =
      let dist = sqrt $ fromIntegral ((x2-x1)^2 + (y2-y1)^2)
      in dist + go rest
    go _ = 0

main :: IO ()
main = do
  n <- readLn
  es <- replicateM n $ do
    [x,y] <- (map read . words) <$> getLine
    return (x,y)
  putStr $ printf "%.1f\n" $ polyPerimeter es
