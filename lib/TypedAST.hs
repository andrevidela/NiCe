{-# LANGUAGE NoImplicitPrelude #-}

module TypedAST where

import Protolude

data ResolvedType = ResolvedType Text
                  | StackType Text
                  | PointerType ResolvedType
                  | MutableType ResolvedType
                  | FunctionType ResolvedType ResolvedType
                  deriving (Show, Eq)

extractFunctionType :: ResolvedType -> Maybe ([ResolvedType], ResolvedType)
extractFunctionType (FunctionType tpe returnType) =
  do (args, ret) <- extractFunctionType tpe
     return (args ++ [ret], returnType)
extractFunctionType tpe = return ([], tpe)

data TypedExpr = TypedIdentifier Text
               | TypedFCall Text ResolvedType [TypedExpr]
               -- The type of the lambda with arguments and return type
               -- This type is usally inferred from the context
               --            |
               --            |  The list of argument names
               --             \             |
               --              \   The list of statements in the body
               --               |           |        |
               --               V           V        V
               | TypedLambda ResolvedType [Text] [TypedStatement]

data TypedStatement = TypedLet Text ResolvedType TypedExpr
                    | WrapTypedExpr TypedExpr

data TypedDeclaration = TypedLetDecl Text ResolvedType (Maybe TypedExpr)
                      | TypedEnum Text [Text]
                      | TypedStruct Text [(Text, ResolvedType)]

data TypedProgram = TypedProgram { statements :: [TypedDeclaration] }
