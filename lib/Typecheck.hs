{-# LANGUAGE NoImplicitPrelude #-}
module Typecheck where

import AST
import Protolude


type TypecheckError = Text

type CheckedAST = [CheckedLet]

data CheckedLet = InferredLet Text InfType
                | VerboseLet Text ExplicitType

newtype InfType = InfType NiceType
newtype ExplicitType = ExplicitType NiceType

data NiceType = NStruct Text [CheckedLet]
              | NEnum Text [EnumCase]
              | NFunc NiceType NiceType
              | Mut NiceType
              | Ptr NiceType
              | Blt BuiltInType

type EnumCase = (Text, Int)

data BuiltInType = BStr
                 | BInt
                 | BFloat
                 | BChar
                 | BUnit
                 | BTop

data StaticTypeDecl = StructDecl Text [EmptyLet]
                    | EnumDecl Text [Text]
checkTypeDecl :: StaticTypeDecl -> NiceType
checkTypeDecl (StructDecl name fields) = undefined
checkTypeDecl (EnumDecl name cases) = NEnum name $ zip cases [0..]

splitDef :: Program -> ([StaticTypeDecl], [LetDecl])
splitDef = foldl combine ([], [])
    where
        combine :: ([StaticTypeDecl], [LetDecl]) -> Definition -> ([StaticTypeDecl], [LetDecl])
        combine (types, lets) (LetDef decl) = (types, decl : lets)
        combine (types, lets) (StructDef name fields) = (StructDecl name fields : types, lets)
        combine (types, lets) (EnumDef name cases) = (EnumDecl name cases : types, lets)


typeCheck :: Program -> Either TypecheckError CheckedAST
typeCheck prog = undefined
