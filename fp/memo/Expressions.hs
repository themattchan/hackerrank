import Control.Applicative

solve :: Int -> [Int] -> String -> Maybe String
solve n [] rops
  | n `mod` 101 == 0 = Just (reverse rops)
  | otherwise = Nothing
solve n r@(n':ns) rops
-- once we reach a mutiple of 101, just multiply the rest of the numbers to keep
-- the property
  | n `mod` 101 == 0 = Just $ reverse rops ++ replicate (length r) '*'
  | otherwise =  solve (n+n') ns ('+':rops)
             <|> solve (n-n') ns ('-':rops)
             <|> solve (n*n') ns ('*':rops)

main :: IO ()
main = do
  _ <- getLine
  n:ns <- map read . words <$> getLine
  case solve n ns "" of
    Nothing -> print "ERROR"
    Just ops -> putStrLn $ concat $ show n : zipWith (:) ops (map show ns)
