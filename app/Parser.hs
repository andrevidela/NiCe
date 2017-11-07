import Lexer
import Data.List
import Data.Either
import Text.Parsec
import Text.Parsec.Prim
import Control.Monad
import Data.Functor.Identity
-- import Control.Applicative


type Parser r = ParsecT [Token] () Maybe r
type Program = [Definition]

data Definition = LetDef LetDecl
                | StructDef Identifier [EmptyLet]
                | EnumDef Identifier [Identifier]

data TypeDecl = TypeDecl { typeName :: String }
data Identifier = Identifier { idName :: String }
data Operator = Operator { opName :: String }

data Expr = FApp Identifier [Expr] 
          | IntLit Int 
          | BoolLit Bool
          | StrLit String 
          | AnonFun [Identifier] Expr 
          | InfixOp Operator Expr Expr
          | PrefixOp Operator Expr
          | PostfixOp Operator Expr


data EmptyLet = EmptyLet 
  { emptyLetID   :: Identifier
  , emptyLetType :: TypeDecl
  }

data ExprLet = ExprLet
  { exprLetID   :: Identifier
  , exprLetType :: TypeDecl
  , exprLetExpr :: Expr
  }

type LetDecl = Either EmptyLet ExprLet

parseLetDecl :: Parser LetDecl
parseLetDecl = undefined

parseLet :: Parser Definition
parseLet = LetDef <$> parseLetDecl 

parseStruct :: Parser Definition
parseStruct = undefined

parseEnum :: Parser Definition
parseEnum = undefined

cleanupWhitespace :: [Token] -> [Token]
cleanupWhitespace ls = undefined

parseProgram :: Parser Program
parseProgram = many $ choice [parseLet, parseStruct, parseEnum]
