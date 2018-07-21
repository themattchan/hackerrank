import Debug.Trace
import Control.Monad
import qualified Data.Map as M

type Idx = (Int, Int) -- row, col
type RIdx = (Level, RPos)

-- start at top left corner, go clockwise.
data Ring = Ring Level RSiz (Int -> Int)
-- position in ring
type RPos = Int
type RSiz = Int
type Level = Int

rotateL :: Int -> Ring -> Ring
rotateL n (Ring lvl siz f) = Ring lvl siz (\i -> f ((i +n) `mod` siz))

-- m rows x n col.
-- coord (m,n) is lower right corner
data MatrixRep = Matrix Int Int

rings :: MatrixRep -> [Ring]
rings = go 1
  where
    go l (Matrix m n)
      | m <= 2 = [ring m n]
      | otherwise = ring m n : go (l+1) (Matrix (m-2) (n-2))
      where
        ring m n = Ring l ((2*m) + (2*n) - 4) (\i -> i)

unrings :: MatrixRep -> [Ring] -> (Idx -> Idx)
unrings (Matrix m n) rs = rPosToIdx . lookupR . idxToRPos
  where
    idxToRPos (x,y) = (lvl, pos)
      where
        (lvl, sm, sn) = idxToLvl 1 m n

        -- there has to be a nice closed form solution
        idxToLvl l m' n' =
          if x == l || x == m' || y == l || y == n'
          then (l, m', n') -- level, size of submatrix
          else idxToLvl (l+1) (m'-1) (n'-1)

        pos = case (x-lvl, y-lvl) of
          (0, j) -> j
          (i,j) | j == sn-1 -> i+j
                | otherwise -> (((sn-1) + (sm-1)) *2) - ( i+j)

    rPosToIdx (l, p) = (l+i,l+j)
      where
        (i,j)= --trace ("level " ++ show l++ "numPosOffs " ++ show (length posOffs) ) $
               --traceShowId
               posOffs !! p
        posOffs = [(0,j) | j <- [0..sn-1]]
                  ++ [(i,sn-1) | i<- [1..sm-1]]
                  ++ [(sm-1,j)|j<-[sn-2, sn-3..0]]
                  ++ [(i,0)|i<-[sm-2,sm-3..1]]
        sm = m - (2*(l-1))
        sn = n - (2*(l-1))

    lookupR (l, p) = (l, ((M.fromList [(lvl, pos) | Ring lvl siz pos <- rs]) M.! l) $ p)

main :: IO ()
main = do
  [m,n, rots]<- map read . words <$> getLine
  inp <- map (map (read :: String -> Int) . words) <$> replicateM m getLine
  let mat = Matrix m n
  let datas = M.fromList [((i,j),e) | (i,row) <- zip [1..] inp, (j,e) <- zip [1..] row]
  -- transform function
  let f = unrings mat . map (rotateL rots) . rings $ mat
  -- putStrLn ""
  -- putStrLn ""
  putStrLn . unlines . map (unwords . map ({-pad . -}show))
    $ [ [ datas M.! f (i,j) | j <- [1..n]] | i <- [1..m] ]


pad s = replicate (3 - length s) ' ' ++ s
