module While where
import Prelude hiding (lookup)
import Control.Arrow ((>>>))
import Control.Applicative hiding (empty, (<|>))
import Control.Monad
import Control.Monad.State hiding (when)

import qualified Data.Map as M
import Data.List (foldl1)

import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String

import Data.Semigroup

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

instance Semigroup Stmt where
  (<>) = Seq

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

sstring = eatSpaces . string

parens :: Parser a -> Parser a
parens = between (char ')') (char ')')

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

parseVar :: Parser Variable
parseVar = many1 letter

parseVal :: Parser Value
parseVal = read <$> many1 digit

parseAOp :: Parser ArithOp
parseAOp =  try (char '*' *> pure Mul)
        <|> try (char '/' *> pure Div)
        <|> try (char '+' *> pure Plus)
        <|> char '-' *> pure Sub

parseROp :: Parser RelOp
parseROp =  try (char '>' *> pure Gt)
        <|> char '<' *> pure Lt

parseBOp :: Parser BoolOp
parseBOp =  try (string "and" *> pure And)
        <|> string "or"  *> pure Or

parseAExpr :: Parser AExpr
parseAExpr =  try (Var  <$> parseVar)
          <|> try (Val  <$> parseVal)
          <|> try (flip AApp <$> parseAExpr <*> parseAOp <*> parseAExpr)
          <|> parens parseAExpr

parseBExpr :: Parser BExpr
parseBExpr =  try (string "true" *> pure TRUE)
          <|> try (string "false" *> pure FALSE)
          <|> try (flip BApp <$> parseBExpr <*> parseBOp <*> parseBExpr)
          <|> try (flip BCmp <$> parseAExpr <*> parseROp <*> parseAExpr)
          <|> parens parseBExpr

parseStmt :: Parser Stmt
parseStmt = foldl1 (<>) <$> sepBy1 parseAll (eatSpaces (char ';'))
  where
    parseAll = try parseIf <|> try parseWhile <|> parseAssign

    parseAssign = do
      v <- parseVar
      sstring ":="
      a <- parseAExpr
      return $ Assign v a

    parseIf = do
      sstring "if"
      b <- parseBExpr
      sstring "then"
      t <- braces parseStmt
      sstring "else"
      f <- braces parseStmt
      return $ If b t f

    parseWhile = do
      sstring "while"
      b <- parseBExpr
      sstring "do"
      s <- braces parseStmt
      return $ While b s

parseProg :: String -> Either ParseError Stmt
parseProg = parse parseStmt ""

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

interpret :: String -> IO ()
interpret = (parseProg >=> evalProg >>> runMachine >>> pure) >>> either pError pResult
  where
    pError  = const $ putStrLn "Parse Error"
    pResult = void . M.traverseWithKey (\v i -> putStrLn $ v ++ " " ++ show i)


test = "fact := 1 ;\nval := 10000 ;\ncur := val ;\nmod := 1000000007 ;\n\nwhile ( cur > 1 )\n  do\n   {\n      fact := fact * cur ;\n      fact := fact - fact / mod * mod ;\n      cur := cur - 1\n   } ;\n\ncur := 0"

main :: IO ()
main = getContents >>= interpret
