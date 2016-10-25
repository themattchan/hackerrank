import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    t' <- readLn
    (resF,_) <- flip execStateT (True,M.empty) $ replicateM_ t' $ do
      [d,r]  <- liftIO $ ((map read . words) <$> getLine :: IO [Int])
      (res,m) <- get
      when res $ do
        case M.lookup d m of
          Just r' -> when (r' /= r) $ do put (False,m)
          Nothing -> put (res, M.insert d r m)
    putStrLn $ if resF then "YES" else "NO"
