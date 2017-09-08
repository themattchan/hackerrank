-- invariant: length picContents == picHeight,
-- where each line has length picWidth
-- furthermore, width must be odd
data Pic = Pic { picWidth :: Int, picHeight :: Int, picContents :: [String] }

beside :: Pic -> Pic -> Pic
beside (Pic w1 h1 ss1) (Pic w2 h2 ss2)
  | h1 == h2 && odd w1  = Pic (w1+w2+1) h1 (zipWith (flip (.) ('_' :) . (++)) ss1 ss2)

above :: Pic -> Pic -> Pic
above (Pic w1 h1 ss1) (Pic w2 h2 ss2)
  | w1 == w2 = Pic w1 (h1+ h2) (ss1 ++ ss2)

centred :: Int -> Pic -> Pic
centred w' (Pic w h s) = Pic w' h s'
  where
    s' = map (pad w') s

pad :: Int -> String -> String
pad w s = sp ++ s ++ sp
  where
    sp = replicate ((w - length s) `div` 2) '_'

drawTriangle :: Int -> Int -> Pic
drawTriangle w h | w >= h
  = Pic w h tri where
      tri = map line [1,3..h]
      line i = pad w $ replicate i '1'

drawNested :: Int -> Int -> Int -> Pic
drawNested d w h
  | d == 0    = drawTriangle w h
  | otherwise = centred w nest `above` (nest `beside` nest)
  where
    nest = drawNested (d-1) (w `div` 2) (h `div` 2)

printPic :: Pic -> IO ()
printPic = putStrLn . unlines . picContents

main :: IO ()
main = do
  n <- readLn
  printPic $ drawNested n 63 63
