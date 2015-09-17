import Control.Monad

pi1 :: [Int]
pi1 = [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3,2,3,8,4,6,2,6,4,3,3,8,3,3]

testPi :: String -> Bool
testPi xs = blerg == prefix
  where blerg  = map length . words $ xs
        prefix = take (length blerg) pi1

main :: IO ()
main = do
  n <- readLn
  let put p | p     = "It's a pi song."
            | not p = "It's not a pi song."
  replicateM_ n $ getLine >>= putStrLn . put . testPi
