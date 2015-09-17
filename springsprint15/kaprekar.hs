module Kaprekar where

good :: Int -> (String, String) -> Bool
good n (a,b) = let a' = read a
                   b' = read b
               in if b' == 0 then False else a' + b' == n

partitions :: Int -> [(String, String)]
partitions n = let w  = show n
                   sp = flip splitAt w
               in init $ map sp [1..length w]

kaprekar :: Int -> Bool
kaprekar x = or . map (good x) . partitions $ (x^2)

main :: IO ()
main = do
  p <- readLn
  q <- readLn

  let nums  =  map kaprekar [p..q]
      prn x = putStr (show x) >> putStr " "

  if p == 1 then putStr "1 " else return ()

  case nums of
   [] -> putStr "INVALID RANGE"
   _  -> mapM_ prn nums

  putStr "\n"
