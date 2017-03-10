-- | Simplify the algebraic expressions
{-

10x + 2x - (3x + 6)/3
18*(2x+2) - 5
((9x + 81)/3 + 27)/3  - 2x
18x + (12x + 10)*(2x+4)/2 - 5x
(2x+5) * (x*(9x + 81)/3 + 27)/(1+1+1)  - 2x
(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1)  - 2x

Sample Output

11x - 2
36x + 31
-x + 18
12x^2 + 47x + 20
2x^3 + 23x^2 + 61x + 45
2x^5 + 5x^4 + 18x^2 + 61x + 45
-}

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Expr
  = Var Char
  | Num Int
  | Mul Expr Expr
  | Div Expr Expr
  | Sub Expr Expr
  | Add Expr Expr
  | Pow Expr Expr
  | Neg Expr
  deriving (Eq, Show)

-- instance Show Expr where
--   show e = case e of
--     Var x     -> [x]
--     Num n     -> show n
--     Mul e1 e2 -> show e1 ++ show e2
--     Div e1 e2 -> show e1 ++ "/"   ++ show e2
--     Sub e1 e2 -> show e1 ++ " - " ++ show e2
--     Add e1 e2 -> show e1 ++ " + " ++ show e2
--     Pow e1 e2 -> show e1 ++ "^"   ++ show e2
--     Neg e1    -> "-" ++ show e1

parser :: Parser Expr
parser = buildExpressionParser optable factor
  where
    binary name fun = Infix  (eatSpaces (string name) >> return fun) AssocRight
    prefix name fun = Prefix (eatSpaces (string name) >> return fun)
    -- tightest binding first
    optable = [ [prefix "-" Neg, prefix "+" id]
              , [binary "*" Mul, binary "/" Div, binary "^" Pow]
              , [binary "+" Add, binary "-" Sub]
              ]

eatSpaces :: Parser a -> Parser a
eatSpaces = (spaces *>) . (<* spaces)

factor :: Parser Expr
factor = try paren <|> try implicitMul <|> try num <|> var
  where
    paren       = between (char '(') (char ')') (eatSpaces parser)
    implicitMul = (Mul <$> num <*> var) <|> (Mul <$> num <*> paren)
    num         = Num . read <$> eatSpaces (many1 digit)
    var         = Var <$> eatSpaces lower

-- | Evaluate purely numeric expressions
eval :: Expr -> Maybe Int
eval (Var _)     = Nothing
eval (Num n)     = Just n
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div e1 e2) = div <$> eval e1 <*> eval e2
eval (Sub e1 e2) = (-) <$> eval e1 <*> eval e2
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Pow e1 e2) = (^) <$> eval e2 <*> eval e2
eval (Neg e)     = (* (-1)) <$> eval e

-- simplConst :: Expr -> Expr
-- simplConst e = case e of
--   Var c     -> e
--   Num i     -> e
--   Mul (Num e1) (Num e2) -> Num (e1 * e2)
--   Mul (Num e1) (Div e2 (Num e3)) -> Div e2 (Num (e1 `div` e3))
--   Div (Num e1) (Num e2) -> Num (e1 `div` e2)
--   Sub e1 e2 ->
--   Add e1 e2 ->
--   Pow e1 e2 ->
--   Pos e1    -> e1
--   Neg (Num e1) -> Num (e1 * (-1))
doParse :: String -> Maybe Expr
doParse = hush . parse parser ""
  where hush (Right e) = Just e
        hush _         = Nothing

main :: IO ()
main = do
  s <- getLine
  print $ parse parser "" s
--  print e
--  print . eval $ e
