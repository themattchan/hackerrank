module While where
import Prelude hiding (lookup)
import Control.Applicative hiding (empty, (<|>))
import Control.Monad
import Control.Monad.State hiding (when)

import qualified Data.Map as M

import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String

--------------------------------------------------------------------------------
-- * Embedding

type Variable = String
type Value = Int

data Stmt
  = Assign Variable AExpr
  | Seq Stmt Stmt
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  deriving (Show)

data ArithOp
  = Plus | Sub | Mul | Div
  deriving (Show)

data BoolOp
  = And | Or
  deriving (Show)

data RelOp
  = Lt | Gt
  deriving (Show)

data AExpr
  = Val Value
  | Var Variable
  | AApp ArithOp AExpr AExpr
  deriving (Show)

data BExpr
  = TRUE | FALSE
  | BApp BoolOp BExpr BExpr
  | BCmp RelOp AExpr AExpr
  deriving (Show)

--------------------------------------------------------------------------------
-- * Parser

eatSpaces :: Parser a -> Parser a
eatSpaces = (spaces *>) . (<* spaces)

parseVar :: Parser Variable
parseVar = many1 letter


-- parser :: Parser Expr
-- parser = buildExpressionParser optable factor
--   where
--     binary name fun = Infix  (eatSpaces (string name) >> return fun) AssocRight
--     prefix name fun = Prefix (eatSpaces (string name) >> return fun)
--     -- tightest binding first
--     optable = [ [prefix "-" Neg, prefix "+" id]
--               , [binary "*" Mul, binary "/" Div, binary "^" Pow]
--               , [binary "+" Add, binary "-" Sub]
--               ]

-- eatSpaces :: Parser a -> Parser a
-- eatSpaces = (spaces *>) . (<* spaces)

-- factor :: Parser Expr
-- factor = try paren <|> try implicitMul <|> try num <|> var
--   where
--     paren       = between (char '(') (char ')') (eatSpaces parser)
--     implicitMul = (Mul <$> num <*> var) <|> (Mul <$> num <*> paren)
--     num         = Num . read <$> eatSpaces (many1 digit)
--     var         = Var <$> eatSpaces lower


-- parseAOp :: Parser ArithOp
-- parseAOp =  char '+' *> pure Plus
--         <|> char '-' *> pure Sub
--         <|> char '*' *> pure Mul
--         <|> char '/' *> pure Div

-- parseBOp :: Parser BoolOp
-- parseBOp =  string "and" *> pure And
--         <|> string "or"  *> pure Or

-- parseROp :: Parser RelOp
-- parseROp = undefined

-- parseAExpr :: Parser AExpr
-- parseAExpr = undefined

-- parseBExpr :: Parser BExpr
-- parseBExpr = undefined

-- parseProg :: Parser Stmt
-- parseProg = undefined

--------------------------------------------------------------------------------
-- * Interpreter

-- This is the state of the interpreter
type Store = M.Map Variable Value
type Machine a = State Store a

runMachine :: Machine a -> Store
runMachine = flip execState M.empty

lookup :: Variable -> Machine Value
lookup = gets . flip (M.!)

assign :: Variable -> Value -> Machine ()
assign v e = modify (M.insert v e)

evalAOp :: ArithOp -> (Int -> Int -> Int)
evalAOp Plus = (+)
evalAOp Sub = (-)
evalAOp Mul = (*)
evalAOp Div = div

evalBOp :: BoolOp -> (Bool -> Bool -> Bool)
evalBOp And = (&&)
evalBOp Or  = (||)

evalROp :: RelOp -> (Int -> Int -> Bool)
evalROp Lt = (<)
evalROp Gt = (>)

evalArith :: AExpr -> Machine Int
evalArith (Val i)         = return i
evalArith (Var v)         = lookup v
evalArith (AApp op e1 e2) = evalAOp op <$> evalArith e1 <*> evalArith e2

evalBool :: BExpr -> Machine Bool
evalBool TRUE               = return True
evalBool FALSE              = return False
evalBool (BApp bop be1 be2) = evalBOp bop <$> evalBool be1  <*> evalBool be2
evalBool (BCmp rop ae1 ae2) = evalROp rop <$> evalArith ae1 <*> evalArith ae2

evalProg :: Stmt -> Machine ()
evalProg (Assign v e) = evalArith e >>= assign v
evalProg (Seq e1 e2)  = evalProg e1 >> evalProg e2
evalProg (If b e1 e2) = do
  b' <- evalBool b
  if b' then evalProg e1
        else evalProg e2
evalProg (While b e)  = do
  b' <- evalBool b
  if b' then evalProg e >> evalProg (While b e)
        else return ()

main :: IO ()
main = return ()
