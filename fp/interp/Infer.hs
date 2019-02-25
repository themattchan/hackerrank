import Prelude hiding (lookup)
import Control.Arrow ((>>>))
import Control.Applicative hiding (empty, (<|>))
import Control.Monad
import Control.Monad.State hiding (when)

import qualified Data.Map as M
import Data.List (foldl1)
import Data.Functor (($>))

import Text.Parsec hiding (State, between, spaces)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

import Data.Semigroup


{-
ident : [_A-Za-z][_A-Za-z0-9]*              // variable names

expr : "let " ident " = " expr " in " expr  // variable defination
     | "fun " argList " -> " expr           // function defination
     | simpleExpr

argList : { 0 or more ident seperated by ' ' }

simpleExpr : '(' expr ')'
           | ident
           | simpleExpr '(' paramList ')'   // function calling

paramList : { 0 or more expr seperated ", " }
-}

type Ident = String
data Exp = Let Ident Exp Exp
         | Fun [Ident] Exp
         | Var Ident
         | App Exp [Exp]

{-

ty : "() -> " ty                            // function without arguments
   | '(' tyList ") -> " ty                  // uncurry function
   | "forall[" argList "]" ty               // generic type
   | simpleTy " -> " ty                     // curry function
   | simpleTy

tyList : { 1 or more ty seperated by ", " }

simpleTy : '(' ty ')'
         | ident
         | simpleTy '[' tyList ']'          // such as list[int]
-}

data TMono = TFunU [TMono] TMono
           | TFunS TMono TMono
           | TApp TMono [TMono]
           | TV Ident
           | Unit

data TPoly = Forall [Ident] TPoly
           | TM TMono

-- pTy :: Parser Ty
-- pTy =

-- algorithm j

generalise :: TMono -> TPoly
generalise ty = Forall (freevars ty) (TM ty)

freevarsMono :: TMono -> [Ident]
freevarsMono = go S.empty where
  go (


--newtype IM a = IM { Int -> (Int, a) }
type TCtx = [(Ident, TPoly)]


infer :: Exp -> IM TPoly
infer (Let x e1 e2) =

env0 =
  ["head: forall[a] list[a] -> a"
  ,"tail: forall[a] list[a] -> list[a]"
  ,"nil: forall[a] list[a]"
  ,"cons: forall[a] (a, list[a]) -> list[a]"
  ,"cons_curry: forall[a] a -> list[a] -> list[a]"
  ,"map: forall[a b] (a -> b, list[a]) -> list[b]"
  ,"map_curry: forall[a b] (a -> b) -> list[a] -> list[b]"
  ,"one: int"
  ,"zero: int"
  ,"succ: int -> int"
  ,"plus: (int, int) -> int"
  ,"eq: forall[a] (a, a) -> bool"
  ,"eq_curry: forall[a] a -> a -> bool"
  ,"not: bool -> bool"
  ,"true: bool"
  ,"false: bool"
  ,"pair: forall[a b] (a, b) -> pair[a, b]"
  ,"pair_curry: forall[a b] a -> b -> pair[a, b]"
  ,"first: forall[a b] pair[a, b] -> a"
  ,"second: forall[a b] pair[a, b] -> b"
  ,"id: forall[a] a -> a"
  ,"const: forall[a b] a -> b -> a"
  ,"apply: forall[a b] (a -> b, a) -> b"
  ,"apply_curry: forall[a b] (a -> b) -> a -> b"
  ,"choose: forall[a] (a, a) -> a"
  ,"choose_curry: forall[a] a -> a -> a"]
