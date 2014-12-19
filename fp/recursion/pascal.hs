pascalRow c r = if c==0 || c==r then 1
				else (pascal c (r-1)) + (pascal (c-1) (r-1))

pascal n = [[ pascalRow c r | c <- [1..r]] | r<- [1..n]]

--iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]

main = do
  n <- read $ readLine
  unlines $
