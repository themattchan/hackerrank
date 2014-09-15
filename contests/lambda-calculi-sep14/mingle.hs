import System.IO
mingle :: String -> String -> String
mingle [] [] = []
mingle (x:xs) (y:ys) = x:y:(mingle xs ys)

main = do
    x <- getLine
    y <- getLine
    putStrLn (mingle x y)
