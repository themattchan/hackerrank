import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Expr =
    Var Char
  | Num Int
  | Mul Expr Expr
  | Div Expr Expr
  | Sub Expr Expr
  | Add Expr Expr
  | Pow Expr Expr
  deriving (Eq)

instance Show Expr where
  show expr = case expr of
    Var x     -> show x
    Num n     -> show n
    Mul e1 e2 -> undefined
    Div e1 e2 -> undefined
    Sub e1 e2 -> show e1 ++ " - " ++ show e2
    Add e1 e2 -> show e1 ++ " + " ++ show e2
    Pow e1 e2 -> show e1 ++ "^" ++ show e2

simpl :: Expr -> Expr
