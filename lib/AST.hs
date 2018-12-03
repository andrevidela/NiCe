{-# LANGUAGE NoImplicitPrelude #-}

module AST where

import           Data.List.NonEmpty
import           Protolude

type Program = [Definition]

data Definition = LetDef LetDecl
                | StructDef Text [EmptyLet]
                | EnumDef Text [Text]
                deriving (Eq, Show)

type LetDecl = Either ExprLet EmptyLet
data EmptyLet = EmptyLet
  { emptyLetID   :: Text
  , emptyLetType :: TypeDecl
  } deriving (Eq, Show)


data ExprLet = ExprLet
  { exprLetID   :: Text
  , exprLetType :: TypeDecl
  , exprLetExpr :: Expr
  } deriving (Eq, Show)

data TypeDecl = SimpleType Text
              | MutableType TypeDecl
              | PointerType TypeDecl
              -- function types have at least one argument type
              | FunctionType (NonEmpty TypeDecl) TypeDecl
                  deriving (Eq, Show)
newtype Identifier = Identifier { idName :: Text } deriving (Eq, Show)

data FArg = FArgument Text | WildCardArg deriving (Eq, Show)

data Expr = FApp Expr [Expr]
          | Proj Expr Text
          | IntLit Int
          | FloatLit Float
          | BoolLit Bool
          | StrLit Text
          | IfExpr Expr Expr Expr
          | AnonFun [FArg] [Statement]
          | InfixOp Text Expr Expr
          | PrefixOp Text Expr
          | PostfixOp Expr Text
          | PlainIdent Text
              deriving (Eq, Show)


data Statement = While Expr [Statement]
               | SLet LetDecl
               | IfStmt Expr [Statement] [Statement]
               | Assign Text Expr
               | Return Expr
               | Plain Expr
                  deriving (Eq, Show)
