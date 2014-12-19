messyMedian lst = median'(0, []) lst
	where
	  median' :: (Int, [Int]) -> [Int] -> [Int]
	  median' (cur, r) []     = reverse r
	  median' (cur, r) (x:xs) = case x <= -1 of
								  True -> median' (cur+1, drop (cur+1+x) r) xs
								  False -> median' (cur+1, x:r) xs
[1,5,-2,3,2,5,4,-7,2,-3]
