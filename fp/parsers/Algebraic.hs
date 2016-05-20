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
