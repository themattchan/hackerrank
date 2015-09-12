module GrahamScan where

import Data.List
import Text.Printf

type Point = (Int, Int)
data Turn = CCW                 -- anti-clockwise / Left turn
          | CW                  -- clockwise      / Right turn
          | CL                  -- collinear
          deriving (Eq)

{- Takes three points and returns the "polar angle" between them.
   if > 0 then they form a counter clockwise turn
   if < 0 then a clockwise turn
   if  = 0 collinear
   magnitude is the size of the angle
-}
ccw :: Point -> Point -> Point -> Double
ccw (x1,y1) (x2,y2) (x3,y3) =
  fromIntegral $ (x2-x1) * (y3-y1) - (y2-y1) * (x3-x1)

whichWay :: Double -> Turn
whichWay ccw
  | ccw < 0   = CW
  | ccw > 0   = CCW
  | otherwise = CL

sortByYMin :: [Point] -> [Point]
sortByYMin = sortBy compareY
  where
    compareY (x1,y1) (x2,y2)
      | y1 == y2 = compare x1 x2
      | otherwise = compare y1 y2

orderCCW :: Point -> Point -> Point -> Ordering
orderCCW mp p1 p2 = let ord = compare (ccw mp p1 p2) 0 in
                     case ord of
                      EQ -> compare (dist mp p1) (dist mp p2)
                      _  -> ord

grahamScan :: [Point] -> [Point]
grahamScan ps' = map fst $ filter (\(_, t) -> t /= CW) $ turns sorteds
  where
    ps = nub ps'
    minP:mins = sortByYMin ps
    sorteds = minP : sortBy (orderCCW minP) mins

turns (p1:rest@(p2:p3:ps)) = (p2, whichWay $ ccw p1 p2 p3) : turns rest
turns _                    = []

dist :: Point -> Point -> Double
dist (x1,y1) (x2,y2) =
  let dx = fromIntegral $ x2-x1
      dy = fromIntegral $ y2-y1 in
  sqrt $ (dx^2) + (dy^2)

perimeter :: [Point] -> Double
perimeter ps = let qs = tail ps ++ [head ps] in
                sum $ zipWith dist ps qs

solve :: [Point] -> Double
solve = perimeter . grahamScan

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans
