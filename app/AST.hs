module AST where

import Data.List.NonEmpty


type Program = [Definition]

data Definition = LetDef LetDecl
                | StructDef String [EmptyLet]
                | EnumDef String [String]
                deriving (Eq, Show)

type LetDecl = Either ExprLet EmptyLet
data EmptyLet = EmptyLet 
  { emptyLetID   :: String
  , emptyLetType :: TypeDecl
  } deriving (Eq, Show)


data ExprLet = ExprLet
  { exprLetID   :: String
  , exprLetType :: TypeDecl
  , exprLetExpr :: Expr
  } deriving (Eq, Show)

data TypeDecl = SimpleType String 
              | MutableType TypeDecl
              | PointerType TypeDecl
              | FunctionType TypeDecl [TypeDecl] TypeDecl
                  deriving (Eq, Show)
data Identifier = Identifier { idName :: String } deriving (Eq, Show)

data Expr = FApp Expr [Expr] 
          | IntLit Int 
          | FloatLit Float
          | BoolLit Bool
          | StrLit String 
          | IfExpr Expr Expr Expr
          | AnonFun [String] [Statement]
          | InfixOp String Expr Expr
          | PrefixOp String Expr
          | PostfixOp Expr String
          | PlainIdent String
              deriving (Eq, Show)


data Statement = While Expr [Statement]
               | SLet LetDecl
               | IfStmt Expr [Statement] [Statement]
               | Return Expr
               | Plain Expr
                  deriving (Eq, Show)
