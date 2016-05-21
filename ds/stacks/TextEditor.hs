import Data.List
import Control.Monad

-- https://en.wikipedia.org/wiki/Rope_(data_structure)
data RopeRoot = Root Int Rope  -- length of whole string
data Rope = Leaf Int String    -- length of stored string
          | Node Int Rope Rope -- length of left string



data Op = Append Int String | Delete Int String -- save
        | Print Int | Undo -- ignore

type State = ([Op], String, Int)

newState = ([], "", 0)

edit :: State -> Op -> IO State
edit st@(acts, buf, buflen) op = case op of
  Append n w -> return (op:acts, buf++w, buflen+n)
  Delete n _ -> let (keep,del) = splitAt (buflen-n) buf in
                  return ((Delete n del):acts, keep, buflen-n)
  Print i    -> do putStrLn [buf !! i]
                   return st
  Undo       -> case acts of
    (Delete n w):acts -> return (acts, buf++w, buflen+n)
    (Append n w):acts -> let (keep,del) = splitAt (buflen-n) buf in
                           return (acts, keep, buflen-n)
    _                 -> undefined

parseOp :: String -> Op
parseOp s = case words s of
    "1":w:_ -> Append (length w) w
    "2":k:_ -> Delete (read k) ""
    "3":k:_ -> Print (read k -1)
    "4":_   -> Undo
    _       -> undefined

main :: IO ()
main = do
  n <- readLn
  foldM_ (\st _ -> do
    op <- getLine
    edit st (parseOp op)
    ) newState [1..n]
