module While where
import Prelude hiding (lookup)
import Control.Arrow ((>>>))
import Control.Applicative hiding (empty, (<|>))
import Control.Monad
import Control.Monad.State hiding (when)

import qualified Data.Map as M
import Data.List (foldl1)

import Text.Parsec hiding (State, between, spaces)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

import Data.Semigroup

--------------------------------------------------------------------------------
-- * Embedding

type Variable = String
type Value = Integer

data Stmt
  = Assign Variable AExpr
  | Seq [Stmt]-- Stmt
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

languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedNames   = [ "if", "then", "else"
                            , "while", "do"
                            , "skip"
                            , "true", "false"
                            , "not", "and", "or"
                            ]
  , Token.reservedOpNames = [ "+", "-", "*", "/", ":="
                            , "<", ">", "and", "or", "not"
                            ]
  }

lexer      = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
braces     = Token.braces     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
spaces     = Token.whiteSpace lexer

binary name fun = Infix  (reservedOp name >> return fun) AssocLeft
prefix name fun = Prefix (reservedOp name >> return fun)

parseAExpr :: Parser AExpr
parseAExpr = buildExpressionParser aOps parseATerm
  where
    aOps = [ [ binary "*" (AApp Mul) , binary "/" (AApp Div) ]
           , [ binary "+" (AApp Plus), binary "-" (AApp Sub) ]
           ]

parseATerm :: Parser AExpr
parseATerm =  parens parseAExpr
          <|> Var <$> identifier
          <|> Val <$> integer

parseBExpr :: Parser BExpr
parseBExpr =  buildExpressionParser bOps parseBTerm
  where
    bOps = [ [ binary "and" (BApp And) , binary "or" (BApp Or) ] ]

parseBTerm :: Parser BExpr
parseBTerm =  parens parseBExpr
          <|> reserved "true"  *> pure TRUE
          <|> reserved "false" *> pure FALSE
          <|> parseRExpr

parseRExpr :: Parser BExpr
parseRExpr = flip BCmp <$> parseAExpr <*> parseROp <*> parseAExpr

parseROp :: Parser RelOp
parseROp =  (reservedOp ">" *> pure Gt)
        <|> (reservedOp "<" *> pure Lt)

parseStmt :: Parser Stmt
parseStmt = parens parseStmt <|> Seq <$> sepBy1 stmts semi
  where
    stmts = parseIf <|> parseWhile <|> parseAssign

    parseAssign = do
      v <- identifier
      reservedOp ":="
      a <- parseAExpr
      return $ Assign v a

    parseIf = do
      reserved "if"
      b <- parseBExpr
      reserved "then"
      t <- braces parseStmt
      reserved "else"
      f <- braces parseStmt
      return $ If b t f

    parseWhile = do
      reserved "while"
      b <- parseBExpr
      reserved "do"
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

evalAOp :: ArithOp -> (Integer -> Integer -> Integer)
evalAOp Plus = (+)
evalAOp Sub = (-)
evalAOp Mul = (*)
evalAOp Div = div

evalBOp :: BoolOp -> (Bool -> Bool -> Bool)
evalBOp And = (&&)
evalBOp Or  = (||)

evalROp :: RelOp -> (Integer -> Integer -> Bool)
evalROp Lt = (<)
evalROp Gt = (>)

evalArith :: AExpr -> Machine Integer
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
evalProg (Seq es)     = mapM_ evalProg es
evalProg (If b e1 e2) = do
  b' <- evalBool b
  if b' then evalProg e1
        else evalProg e2
evalProg (While b e)  = do
  b' <- evalBool b
  when b' (evalProg e >> evalProg (While b e))

interpret :: String -> IO ()
interpret = (parseProg >=> evalProg >>> runMachine >>> pure) >>> either pError pResult
  where
    pError  = const $ putStrLn "Parse Error"
    pResult = void . M.traverseWithKey (\v i -> putStrLn $ v ++ " " ++ show i)

test = unlines
  [ "fact := 1 ;"
  , "val := 10000 ;"
  , "cur := val ;"
  , "mod := 1000000007 ;"
  , ""
  , "while ( cur > 1 )"
  , "  do"
  , "   {"
  , "      fact := fact * cur ;"
  , "      fact := fact - fact / mod * mod ;"
  , "      cur := cur - 1"
  , "   } ;"
  , ""
  , "cur := 0"
  ]


main :: IO ()
main = getContents >>= interpret
