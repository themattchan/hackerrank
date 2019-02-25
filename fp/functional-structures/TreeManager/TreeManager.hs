{-# LANGUAGE TypeApplications,ViewPatterns,BangPatterns #-}
import Data.List (splitAt)

-- tree zipper: current subtree with stack of paths
type TZ = (T, [DT])

data T = T Int [T]  deriving Show

-- LR zipper of a level
data DT = DT Int [T] [T]  deriving Show

tz0 = (T 0 [], [])

interp :: TZ -> String -> IO TZ
interp (T n kids, dts) s = case words s of
  ["change", (read @Int -> x)]
    -> pure (T x kids, dts)

  ["print"]
    -> print n *> pure (T n kids, dts)

  ["visit", "left"]
    | (DT dn (l:ls) rs : dts') <- dts
      -> pure (l, DT dn ls (T n kids : rs) : dts')

  ["visit", "right"]
    | (DT dn ls (r:rs) : dts') <- dts
      -> pure (r, DT dn (T n kids : ls) rs : dts')

  ["visit", "parent"]
    | (DT dn ls rs : dts') <- dts
      -> pure (T dn (reverse ls ++ T n kids : rs), dts')

  ["visit", "child", (read @Int -> x)]
    | length kids >= x ->
        let (ls, c:rs) = splitAt (x-1) kids in
          pure (c, DT n (reverse ls) rs : dts)

  ["insert", "left", (read @Int -> x)]
    | (DT dn ls rs : dts') <- dts
      -> pure (T n kids, DT dn (T x [] : ls) rs : dts')

  ["insert", "right", (read @Int -> x)]
    | (DT dn ls rs : dts') <- dts
      -> pure (T n kids, DT dn ls (T x [] : rs) : dts')

  ["insert", "child", (read @Int -> x)]
    -> pure (T n (T x [] : kids), dts)

  ["delete"]
    | (DT dn ls rs : dts') <- dts
      -> pure ( T dn (reverse ls ++ rs), dts')

  _ -> error $ "bad op: "++ s ++ "\n\n" ++ show (T n kids, dts)


main = do
  n <- readLn @Int
  let go !n tz | n == 0 = pure ()
               | otherwise = getLine >>= interp tz >>= go (n-1)
  go n tz0
