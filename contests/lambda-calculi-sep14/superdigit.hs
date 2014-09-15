import Data.Char

superDigit :: String -> Int -> String
superDigit n p = reduce (show((read (reduce n)) * p))
	where
	  reduce :: String -> String
	  reduce xs
		  | length xs == 1 =  xs
		  | otherwise = reduce $ show $ (sum $ map digitToInt xs)

main = do
  inp <- getLine
  let (n,p) = (head $ words inp, read $ head $ tail $ words inp :: Int)
  putStrLn (superDigit n p)
