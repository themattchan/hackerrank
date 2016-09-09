import Control.Applicative hiding ((<|>))
import Data.Bits
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

{-
Precedence
1. Unary ops, brackets
2. Mult/div
3. Add/sub

 Expression ::= Term [+-] Expression
              | Term

 Term       ::= Factor [*/] Term
              | Factor

 Factor     ::= Number
              | [+-] Factor
              | '(' Expression ')'

-}

import Data.Bits
import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Expr
  = Num Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Integer
  | Pos Expr
  | Neg Expr
  deriving (Eq, Show)

-- cribbed from
-- https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Expr.html
-- have to specify: root of tree `expr`, leaves `factor`.
expr :: Parser Expr
expr = buildExpressionParser optable factor
  where
    binary name fun = Infix  (eatSpaces (string name) >> return fun) AssocRight
    prefix name fun = Prefix (eatSpaces (string name) >> return fun)
    -- tightest binding first
    optable = [ [prefix "-" Neg, prefix "+" Pos]
              , [binary "*" Mul, binary "/" Div]
              , [binary "+" Add, binary "-" Sub]
              ]

eatSpaces :: Parser a -> Parser a
eatSpaces p = spaces *> p <* spaces

factor :: Parser Expr
factor = try paren <|> num
  where paren = between (char '(') (char ')')
                        (eatSpaces expr)
        num   = Num . read <$> eatSpaces (many1 digit)

doParse :: String -> Maybe Expr
doParse = hush . parse expr "" 
  where hush (Left _) = Nothing
        hush (Right e) = Just e

eval :: Expr -> Integer
eval e = case e of
  Num i     -> toInteger i `mod` p
  Add e1 e2 -> ((eval e1 `mod` p) + (eval e2 `mod` p)) `mod` p
  Sub e1 e2 -> ((eval e1 `mod` p) - (eval e2 `mod` p)) `mod` p
  Mul e1 e2 -> ((eval e1 `mod` p) * (eval e2 `mod` p)) `mod` p
  Div e1 e2 -> eval (Mul e1 (Exp e2 (p-2)))
  Exp e' n  -> modExp (eval e') n p
  Pos e'    -> eval e' `mod` p
  Neg e'    -> negate (eval e') `mod` p

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
         where t = if testBit e 0 then b `mod` m else 1

p :: Integer
p = 1000000007

main :: IO ()
main = do
  s <- getLine
  let Just e = doParse s
--  print e
  print . eval $ e
