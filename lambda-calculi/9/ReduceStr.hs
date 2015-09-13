import Data.List
reduce_str str = nub $ concat $ map (\x -> head x : []) (group str)

main = do
  str  <- getLine
  putStrLn (reduce_str str)
