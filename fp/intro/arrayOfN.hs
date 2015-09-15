fn = flip replicate 1

main = readLn >>= print . length . fn
