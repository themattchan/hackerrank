good :: Int -> (String, String) -> Bool
good n (a,b) = let a' = read a
                   b' = read b
             in case b' of
                 0 -> False
                 _ -> a' + b' == n

split n = let w = show n in
           [(take x w, drop x w) | x<-[1..((length w)-1)]]

kaprekar :: Int -> Bool
kaprekar x = or . map (good x) $ split (x^2)

main :: IO ()
main = do
  p <- readLn
  q <- readLn

  let nums = [x | x<-[p..q], kaprekar x == True]
      prn x = (putStr $ show x) >> putStr " "

  if p == 1 then putStr "1 " else return ()

  case nums of
   [] -> putStr "INVALID RANGE"
   _  -> mapM_ prn nums

  putStr "\n"
