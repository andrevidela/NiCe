

module AbsGrammar where

-- Haskell module generated by the BNF converter




newtype TypeID = TypeID ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype Id = Id String deriving (Eq, Ord, Show, Read)
newtype OperatorSymbol = OperatorSymbol String
  deriving (Eq, Ord, Show, Read)
data Program = PDefs [Def]
  deriving (Eq, Ord, Show, Read)

data Def
    = DStruct TypeID [LetDecl] | DEnum TypeID [SumType] | DLetl LetAss
  deriving (Eq, Ord, Show, Read)

data SumType = EEmpty Id | EProd TypeID [TypeDecl]
  deriving (Eq, Ord, Show, Read)

data Stmt
    = SLetDecl LetDecl
    | SLetAss LetAss
    | SAss Id Expr
    | SWhile Expr [Stmt]
    | SRet Expr
    | SExpr Expr
  deriving (Eq, Ord, Show, Read)

data LetDecl = LetDeclar Id TypeDecl
  deriving (Eq, Ord, Show, Read)

data LetAss = LetAssgn LetDecl Expr
  deriving (Eq, Ord, Show, Read)

data PostFix = ProdPorj Expr | Deref
  deriving (Eq, Ord, Show, Read)

data Expr
    = Postfix Expr PostFix
    | Closure [Id] [Stmt]
    | FApp Id [Expr]
    | IdExpr Id
    | EInt Integer
    | EDouble Double
    | ETrue
    | EFalse
    | ETimes Expr Expr
    | EDiv Expr Expr
    | EPlus Expr Expr
    | EMinus Expr Expr
    | ELt Expr Expr
    | EGt Expr Expr
    | ELtEq Expr Expr
    | EGtWq Expr Expr
    | EEq Expr Expr
    | ENEq Expr Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | IfExpr Expr Expr Expr
  deriving (Eq, Ord, Show, Read)

data TypeMod = MutMod | PtrMod
  deriving (Eq, Ord, Show, Read)

data TypeTkn = TVoid | TUnit | TID TypeID
  deriving (Eq, Ord, Show, Read)

data Space = TWhitespace
  deriving (Eq, Ord, Show, Read)

data Operator = TInfixOp OperatorSymbol | TPrefixOp OperatorSymbol
  deriving (Eq, Ord, Show, Read)

data OPerator = TPostfixOp OperatorSymbol
  deriving (Eq, Ord, Show, Read)

data TypeDecl
    = ModTpe [TypeMod] TypeTkn | FunType [TypeDecl] TypeDecl
  deriving (Eq, Ord, Show, Read)

