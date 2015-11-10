module While where
import Control.Applicative hiding (empty, (<|>))
import qualified Data.List as Lst ((\\))
import Data.Map hiding (foldl, foldr)
import Control.Monad.State hiding (when)
import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String

-- TYPES
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

-- This is the state of the interpreter
type Store = Map Variable Value
