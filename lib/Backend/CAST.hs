{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.CAST where

import Backend.CTypes
import TypedAST
import Protolude


concatUnzip :: [([a], [b])] -> ([a], [b])
concatUnzip ls = let (f, s) = unzip ls in (concat f, concat s)

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd _ [] = []
mapSnd f ((a, b) : xs) = (a, f b) : mapSnd f xs

compileExpr :: TypedExpr -> CExpr
compileExpr = undefined

compileType :: ResolvedType -> CType
compileType (ResolvedType name) = CConst $ NamedType name
compileType (StackType name) = CConst $ NamedType name
compileType (PointerType tpe) = CConst $ CPointer (compileType tpe)
compileType (FunctionType arg ret) = undefined
compileType (MutableType (ResolvedType name)) = NamedType name
compileType (MutableType (StackType name)) = NamedType name
compileType (MutableType (PointerType tpe)) = CPointer (compileType tpe)
compileType (MutableType (FunctionType _ _)) = undefined
compileType (MutableType (MutableType tpe)) = compileType tpe

compileStructInit :: Text -> Text -> CType -> [TypedExpr] -> [CStatement]
compileStructInit varName fnName varType args =
  [ VariableDecl varName varType
  , ExprAsStatment $ FunctionCall fnName ((CRef (CIdentifier varName)) : (map compileExpr args))]

compileStatment :: TypedStatement -> [CStatement]
compileStatment (WrapTypedExpr expr) = [ ExprAsStatment (compileExpr expr) ]
compileStatment (TypedLet name (StackType letType) (TypedFCall fnName _ args)) =
  compileStructInit name fnName (NamedType letType) args
compileStatment (TypedLet name varType expr) =
  [ VariableDecl name (compileType varType)
  , VariableAssign name (compileExpr expr) ]

getReturnType :: ResolvedType -> CType
getReturnType = undefined

compileDecl :: TypedDeclaration -> CDeclaration
-- Compiling a function to the top level
compileDecl (TypedLetDecl name tpe (Just (TypedLambda _ args body))) =
  FunctionDeclaration name (getReturnType tpe) [] (body >>= compileStatment)

compileDecl (TypedLetDecl name tpe val) = GlobalVarDeclaration name (compileType tpe) (compileExpr <$> val)
compileDecl (TypedEnum name fields) = EnumDeclaration name fields
compileDecl (TypedStruct name fields) = StructDeclaration name (mapSnd compileType fields)

compileC :: TypedProgram -> CProgram
compileC (TypedProgram s) = CProgram $ map compileDecl s

