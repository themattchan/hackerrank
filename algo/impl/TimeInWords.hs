main :: IO ()
main = time <$> readLn <*> readLn >>= putStrLn

time :: Int -> Int -> String
time hr mins = unwords (ss hr mins)
  where
    norm m | m > 30    = 60 - m
           | otherwise = m

    distS m h | m > 30    = ["to", units (mod (h+1) 12)]
              | otherwise = ["past", units h]

    minS m | m == 15   = ["quarter"]
           | m == 30   = ["half"]
           | otherwise = minS1 m ++ [minS2 m]

    minS1 m | m < 16    = [units m]
            | m < 20    = [units m' ++ "teen"]
            | otherwise = ["twenty", units m']
      where m' = mod m 10

    minS2 1 = "minute"
    minS2 _ = "minutes"

    ss h m | m == 0    = units h : ["o' clock"]
           | otherwise = minS (norm m) ++ distS m h

    units = (!!) ["","one", "two","three","four","five","six","seven","eight","nine","ten"
                 ,"eleven","twelve","thirteen","fourteen","fifteen"]
