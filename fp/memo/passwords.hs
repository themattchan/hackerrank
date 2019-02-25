import Data.List
import Control.Monad
import Control.Arrow
import Data.Maybe

main = do
  t <- readLn
  replicateM t $ do
    _ <- getLine
    passes <- words <$> getLine
    inp <- getLine
    putStrLn $ maybe "WRONG PASSWORD" unwords (go [] passes inp)

go xx ws "" = Just (reverse xx)
go xx ws i = join
             . fmap (\(w, Just tl) -> go (w:xx) ws tl)
             . find (isJust.snd)
             . map (id &&& (`stripPrefix` i))
             $ ws
