module AST where

import Data.List.NonEmpty


type Program = [Definition]

data Definition = LetDef LetDecl
                | StructDef String [EmptyLet]
                | EnumDef String [String]

type LetDecl = Either EmptyLet ExprLet
data EmptyLet = EmptyLet 
  { emptyLetID   :: String
  , emptyLetType :: TypeDecl
  }

data ExprLet = ExprLet
  { exprLetID   :: String
  , exprLetType :: TypeDecl
  , exprLetExpr :: Expr
  }

data TypeDecl = SimpleType String 
              | FunctionType TypeDecl [TypeDecl] TypeDecl
data Identifier = Identifier { idName :: String }

data Expr = FApp Expr [Expr] 
          | IntLit Int 
          | BoolLit Bool
          | StrLit String 
          | AnonFun [String] [Statement]
          | InfixOp String Expr Expr
          | PrefixOp String Expr
          | PostfixOp String Expr


data Statement = While Expr [Statement]
               | SLet LetDecl
               | Return Expr
               | Plain Expr
