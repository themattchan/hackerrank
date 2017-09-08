-- invariant: length picContents == picHeight,
-- where each line has length picWidth
-- furthermore, width must be odd
--data Pic = Pic { picWidth :: Int, picHeight :: Int, picContents :: [String] }
newtype Pic = Pic { getPic :: [String] }

beside :: Pic -> Pic -> Pic
beside (Pic p1) (Pic p2) = Pic $ zipWith (flip (.) ('_' :) . (++)) p1 p2

above :: Pic -> Pic -> Pic
above (Pic p1) (Pic p2) = Pic (p1 ++ p2)

centred :: Int -> Pic -> Pic
centred w (Pic p) = Pic (map (pad w) p)

pad :: Int -> String -> String
pad w s = sp ++ s ++ sp
  where
    sp = replicate ((w - length s) `div` 2) '_'

drawTriangle :: Int -> Int -> Pic
drawTriangle w h
  | w >= h = Pic tri
  where
    tri = map line [1,3..h]
    line i = pad w $ replicate i '1'

drawNested :: Int -> Int -> Int -> Pic
drawNested d w h
  | d == 0    = drawTriangle w h
  | otherwise = centred w nest `above` (nest `beside` nest)
  where
    nest = drawNested (d-1) (w `div` 2) (h `div` 2)

printPic :: Pic -> IO ()
printPic = putStrLn . unlines . getPic

main :: IO ()
main = do
  n <- readLn
  printPic $ drawNested n 63 63
