--import Control.Applicative hiding (<|>)
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

data Expr =
    Num Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pos Expr
  | Neg Expr

-- cribbed from
-- https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Expr.html
-- have to specify: root of tree `expr`, leaves `factor`.
expr :: Parser Expr
expr = buildExpressionParser optable factor
  where
    binary  name fun = Infix   ( reservedOp haskell name >> return fun ) AssocLeft
    prefix  name fun = Prefix  $ reservedOp haskell name >> return fun
    postfix name fun = Postfix $ reservedOp haskell name >> return fun
    -- tightest binding first
    optable = [ prefix "-" Neg
              , prefix "+" Pos
              , binary "*" Mul
              , binary "/" Div
              , binary "+" Add
              , binary "-" Sub
              ]

factor = paren <|> num <|> spaces
  where paren = between (char '(') (char ')') expr
        num   = Num (read <$> many1 digit)

doParse s = case parse expr "" s of
  Left _  -> Nothing
  Right e -> Just e

eval :: Expr -> Integer
eval expr = case expr of
  Num i     -> i `mod` bigmod
  Add e1 e2 -> (eval e1 + eval e2) `mod` bigmod
  Sub e1 e2 -> (eval e1 - eval e2) `mod` bigmod
  Mul e1 e2 -> (eval e1 * eval e2) `mod` bigmod
  Div e1 e2 -> (eval e1 `div` eval e2) `mod` bigmod
  Pos e     -> (eval e) `mod` bigmod
  Neg e     -> (negate $ eval e)  `mod` bigmod


bigmod = 1000000007 :: Integer

main = do
  Just e <- getLine >>= doParse
  putStrLn . show . eval $ e
