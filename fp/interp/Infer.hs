import Control.Monad
import Data.Either
import qualified Text.Parsec
import Text.Parsec hiding (runParser)
import Text.Parsec.String(Parser(..))
import Text.Parsec.Char
import Text.Parsec.Combinator
import qualified Data.Map as M

runParser :: Parser a -> String -> Either Text.Parsec.ParseError a
runParser p = Text.Parsec.runParser p () ""

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

data Expr = Let String Expr Expr
          | Fun [String] Expr
          | Id String
          | Call Expr [Expr]

parseIdent :: Parser String
parseIdent = (:) <$>(try upper<|>try lower<|>try (char '_'))<*> many (try alphaNum<|>try (char '_'))

parens = between (char '(') (char ')')
brackets = between (char '[') (char ']')

parseArgList = sepBy parseIdent (string " ")

parseExpr :: Parser Expr
parseExpr = parseLet <|> parseFun <|> parseSimple
  where
    parseLet = do
      string "let "
      i <- parseIdent
      string " = "
      e <- parseExpr
      string " in "
      b <- parseExpr
      return $ Let i e b

    parseFun = do
      string "fun "
      args <- parseArgList
      string " -> "
      e <- parseExpr
      return $ Fun args e

    parseSimple = parens parseExpr <|> (Id <$> parseIdent) <|> parseFuncall
      where
        parseFuncall = do
          e <- parseSimple
          args <- parens $ sepBy parseExpr (string ", ")
          return $ Call e args


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
-- data Poly = Forall [TVar] Ty
-- newtype TVar = TV String
data Ty = [Ty] :=> Ty
        | TCtor Ty [Ty]
        | TId String
        | TPoly [String] Ty
        deriving (Eq, Show)


parseTy :: Parser Ty
parseTy = try parseFun <|> try parsePoly <|> try parseCurriedFun <|> try parseSimpleTy
  where
    parseFun = do
      argTys <- parens $ sepBy parseTy (string ", ")
      string " -> "
      retTy <- parseTy
      return $ argTys :=> retTy

    parseCurriedFun =  do
      a <- parseSimpleTy
      string " -> "
      b <- parseTy
      return $ [a] :=> b

    parsePoly = do
      string "forall"
      args <- brackets parseArgList
      space
      ty <- parseTy
      return $ TPoly args ty

    parseSimpleTy = try (parens parseTy) <|> (TId <$> parseIdent)<|> try parseTCtor
      where
        parseTCtor = do
          ct <- parseSimpleTy
          args <- brackets $ sepBy1 parseTy (string ", ")
          return $ TCtor ct args

type TEnv = M.Map String Ty

initialTEnv :: TEnv
initialTEnv = M.unions $ either mempty id $ runParser (go `sepEndBy` newline) env
  where
    go = do
      s <- parseIdent
      string ": "
      ty <- parseTy
      return (M.singleton s ty)

    env = "head: forall[a] list[a] -> a\n\ntail: forall[a] list[a] -> list[a]\nnil: forall[a] list[a]\ncons: forall[a] (a, list[a]) -> list[a]\ncons_curry: forall[a] a -> list[a] -> list[a]\nmap: forall[a b] (a -> b, list[a]) -> list[b]\nmap_curry: forall[a b] (a -> b) -> list[a] -> list[b]\none: int\nzero: int\nsucc: int -> int\nplus: (int, int) -> int\neq: forall[a] (a, a) -> bool\neq_curry: forall[a] a -> a -> bool\nnot: bool -> bool\ntrue: bool\nfalse: bool\npair: forall[a b] (a, b) -> pair[a, b]\npair_curry: forall[a b] a -> b -> pair[a, b]\nfirst: forall[a b] pair[a, b] -> a\nsecond: forall[a b] pair[a, b] -> b\nid: forall[a] a -> a\nconst: forall[a b] a -> b -> a\napply: forall[a b] (a -> b, a) -> b\napply_curry: forall[a b] (a -> b) -> a -> b\nchoose: forall[a] (a, a) -> a\nchoose_curry: forall[a] a -> a -> a\n"

main = return ()
