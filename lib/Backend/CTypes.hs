{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.CTypes where

import Protolude

data CType = NamedType Text
           | CPointer CType
           | CConst CType
           | CArray CType
           | CFuncPtr [CType] CType
           | CUnit -- void

data CExpr = CInt Int
           | CText Text
           | CChar Char
           | CRef CExpr
           | CDeref CExpr
           | FunctionCall Text [CExpr]
           | CIdentifier Text

data CStatement
  -- test, body
  = CWhile CExpr [CStatement]
  -- name and type
  | VariableDecl Text CType
  -- name, type, value
  | VariableAssign Text CExpr
  -- and expression with a side effect, returns void
  | ExprAsStatment CExpr


-- Top level declaration in C
data CDeclaration
  -- name, list of fields
  = StructDeclaration Text [(Text, CType)]
  -- name, list of values
  | EnumDeclaration Text [Text]
  -- name, type, body
  | GlobalVarDeclaration Text CType (Maybe CExpr)
  -- name, return type, arguments, body
  | FunctionDeclaration Text CType [(Text, CType)] [CStatement]


--  A C Program is a main function and a list of declarations
data CProgram = CProgram [CDeclaration]

test :: CProgram
test = CProgram [ FunctionDeclaration "main" CUnit [("args", CPointer (NamedType "char"))]
                  [ CWhile (CInt 1)
                    [ ExprAsStatment $ FunctionCall "printf" [CText "hello world"]
                    , VariableDecl "i" (NamedType "int")
                    , VariableAssign "i" (CInt 3)
                    ]
                  ]
                ]


