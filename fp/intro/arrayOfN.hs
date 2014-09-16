fn n = replicate n 1

main = do
n <- readLn :: IO Int
print (length(fn(n)))
