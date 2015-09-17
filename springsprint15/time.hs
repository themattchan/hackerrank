mins :: [String]
mins =
 [  ""
 , "one"
 , "two"
 , "three"
 , "four"
 , "five"
 , "six"
 , "seven"
 , "eight"
 , "nine"
 , "ten"
 , "eleven"
 , "twelve"
 , "thirteen"
 , "fourteen"
 , "fifteen"
 , "sixteen"
 , "seventeen"
 , "eighteen"
 , "nineteen"
 , "twenty"
 ]

hours :: [String]
hours = (take 13 mins) ++ ["one"]

time :: Int -> Int -> String
time h m
  | m == 0    = (hours !! h)    ++ " o' clock"
  | m == 15   = "quarter past " ++ (hours !! h)
  | m == 30   = "half past "    ++ (hours !! h)
  | m == 45   = "quarter to "   ++ (hours !! (h+1))
  | m <= 20   = (mins !! m)
                ++ (if m == 1 then " minute past " else " minutes past ")
                ++ (mins !! h)
  | m <  30   = "twenty " ++ (mins !! (m-20)) ++ " minutes past "
                ++ (hours !! h)
  | otherwise = pastHour (60-m)
    where
      pastHour m
        | m <= 20 = (mins !! m)
                    ++ (if m == 1 then " minute to " else " minutes to ")
                    ++ (hours !! (h+1))
        | m < 30  = "twenty " ++ (mins !! (m-20)) ++ " minutes to "
                     ++ (hours !! (h+1))

main :: IO ()
main = do
  h <- readLn
  m <- readLn
  putStrLn $ time h m
