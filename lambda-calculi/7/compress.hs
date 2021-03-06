import Data.List
compress str = concat $ map comp (group str)
    where
      comp s@(x:_:_) = x : (show $ length s)
      comp s@(x:_)   = s

main = do
  str  <- getLine
  putStrLn $ compress str
