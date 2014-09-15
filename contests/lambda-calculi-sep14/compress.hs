import Data.List
compress str = concat $ map comp (group str)
    where
      comp s@(x:xs) = if length s == 1 then s
                      else x:(show (length s))

main = do
  str  <- getLine
  putStrLn (compress str)
