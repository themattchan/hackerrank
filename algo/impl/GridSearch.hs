import Control.Monad
import Data.Bool
import Data.List

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
      [big, small] <- replicateM 2 $ do
        r <- read . head .  words <$> getLine
        replicateM r getLine
      putStrLn $ bool "NO" "YES" (search small big)

search :: [String] -> [String] -> Bool
search (n:ns) =
  any (\(h:hs) -> any (\i -> matchRest ns (map (drop i) hs)) (findStarts n h)) . init . tails
  where
    findStarts targ s =
      [ n
      | (n,t) <- zip [0..] (tails s)
      , targ `isPrefixOf` t
      ]

    matchRest ns hs = length hs >= length ns && and (zipWith isPrefixOf ns hs)
