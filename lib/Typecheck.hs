{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Typecheck where

import           AST
import qualified Data.Map as Map
import           Protolude hiding (TypeError)


type TypecheckError = Text

type CheckedAST = [CheckedLet]

data CheckedLet = InferredLet Text InfType
                | VerboseLet Text ExplicitType
                deriving (Eq, Show)

newtype InfType = InfType NiceType deriving (Eq, Show)
fromInf :: InfType -> NiceType
fromInf (InfType t) = t

newtype ExplicitType = ExplicitType NiceType deriving (Eq, Show)

data NiceType = NStruct Text [CheckedLet]
              | NEnum Text [EnumCase]
              | NFunc (NonEmpty NiceType) NiceType
              | Mut NiceType
              | Ptr NiceType
              | Blt BuiltInType
              deriving (Eq, Show)

type EnumCase = (Text, Int)

data BuiltInType = BStr
                 | BInt
                 | BFloat
                 | BChar
                 | BUnit
                 | BTop
                 deriving (Eq, Show)

data StaticTypeDecl = StructDecl Text [EmptyLet]
                    | EnumDecl Text [Text]

type Environement = (Map.Map Text NiceType)

declToNice :: Environement -> TypeDecl -> NiceType
declToNice e (SimpleType name) = undefined -- error
declToNice e (MutableType tpe) = undefined
declToNice e (PointerType tpe) = undefined
declToNice e (FunctionType args ret) = undefined

checkTypeDecl :: StaticTypeDecl -> NiceType
checkTypeDecl (EnumDecl name cases)    = NEnum name $ zip cases [0..]
checkTypeDecl (StructDecl name fields) = NStruct name (fmap checkLet fields)
    where checkLet :: EmptyLet -> CheckedLet
          checkLet (EmptyLet name tpe) = VerboseLet name (ExplicitType (declToNice mempty tpe))

splitDef :: Program -> ([StaticTypeDecl], [LetDecl])
splitDef = foldl combine ([], [])
    where
        combine :: ([StaticTypeDecl], [LetDecl]) -> Definition -> ([StaticTypeDecl], [LetDecl])
        combine (types, lets) (LetDef decl) = (types, decl : lets)
        combine (types, lets) (StructDef name fields) = (StructDecl name fields : types, lets)
        combine (types, lets) (EnumDef name cases) = (EnumDecl name cases : types, lets)

inferExpr :: Environement -> Expr -> InfType
inferExpr e exp = undefined

matches :: TypeDecl -> NiceType -> Text -> Maybe TypecheckError
matches (SimpleType n1) (NStruct n2 _) _ | n1 == n2 = Nothing
matches (SimpleType n1) (NEnum n2 _)   _ | n1 == n2 = Nothing
matches (PointerType tpe1) (Ptr tpe2) n = matches tpe1 tpe2 n
matches (MutableType lhs) (Mut rhs) n = matches lhs rhs n
matches (FunctionType argsl retl) (NFunc argsr retr) name =
    checkArgs name (toList argsl) (toList argsr) <|> matches retl retr name
    where checkArgs :: Text -> [TypeDecl] -> [NiceType] -> Maybe TypecheckError
          checkArgs _ [] [] = Nothing
          checkArgs name (l : ls) (r : rs) = matches l r name <|> checkArgs name ls rs
          checkLength name l r | length l == length r = Nothing
                               | otherwise = Just $ argumentLengthError name (length l) (length r)
matches lhs rhs name = Just $ typeMismatch name lhs rhs

argumentLengthError :: Text -> Int -> Int -> TypecheckError
argumentLengthError fn expected actual = "Function '" <> fn 
    <> "' expects " <> show expected <> " arguments, but got "
    <> show actual <> " instead."

typeMismatch :: Text -> TypeDecl -> NiceType -> TypecheckError
typeMismatch name expected actual = "Type mismatch for variable '" <> name 
    <> "' expected type '" <> show expected
    <> "' but got type '" <> show actual <> "' instead."

checkLetBinding :: Environement -> ExprLet -> Either TypecheckError CheckedLet
checkLetBinding e (ExprLet name tpe expr) = maybe 
    (Right . VerboseLet name . ExplicitType $ declToNice e tpe)
    Left
    ((tpe `matches` fromInf (inferExpr e expr)) name)


typeCheck :: Program -> Either TypecheckError CheckedAST
typeCheck prog = undefined
