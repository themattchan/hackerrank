solve :: Double -> Double
solve  = (1+) . sum . e
  where e x = [(x**i)/(product [1..i]) | i <- [1..9]]

main :: IO ()
main = getContents >>=
       mapM_ print . map (solve . read) . tail . words
