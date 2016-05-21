import Control.Applicative hiding ((<|>))
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

factor = try paren <|> num
  where paren = between (char '(') (char ')')
                        (eatSpaces expr)
        num   = Num . read <$> (eatSpaces $ many1 digit)

doParse s = case parse expr "" s of
  Left _  -> Nothing
  Right e -> Just e

eval :: Expr -> Integer
eval expr = case expr of
  Num i     -> toInteger i `mod` bigmod
  Add e1 e2 -> ((eval e1 `mod` bigmod) + (eval e2 `mod` bigmod)) `mod` bigmod
  Sub e1 e2 -> ((eval e1 `mod` bigmod) - (eval e2 `mod` bigmod)) `mod` bigmod
  Mul e1 e2 -> ((eval e1 `mod` bigmod) * (eval e2 `mod` bigmod)) `mod` bigmod
  Div e1 e2 -> error "TODO"
  Pos e     -> (eval e) `mod` bigmod
  Neg e     -> (negate $ eval e)  `mod` bigmod


bigmod = 1000000007

main = do
  s <- getLine
  let Just e = doParse s
  print e
  print . eval $ e
