{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.CTypes where

import Protolude

data CType
  = NamedType Text
  | CPointer CType
  | CConst CType
  | CArray CType
  | CFuncPtr [CType] CType
  | CUnit -- void

data CExpr
  -- literal int, ex: 3
  = CInt Int
  -- literal string, ex: "abc"
  | CText Text
  -- literal char, ex: 'a'
  | CChar Char
  -- address of expression, ex: &name
  | CRef CExpr
  -- value pointed by address, ex: *name
  | CDeref CExpr
  -- function application, ex: f(a,b,c)
  | FunctionCall Text [CExpr]
  -- plain identifier, ex: name
  | CIdentifier Text
  -- arbitrary infix function, ex: 3 + 5
  | InfixFn Text CExpr CExpr

data CStatement
  -- test, body
  = CWhile CExpr [CStatement]
  -- name and type
  | VariableDecl Text CType
  -- name, value
  | VariableAssign Text CExpr
  -- name type and value
  | VariableInit Text CType CExpr
  -- an expression with a side effect, returns void
  | ExprAsStatment CExpr
  -- a return statement, breaks the control flow
  | ReturnStatement CExpr
  -- conditional branching
  | IfStatement CExpr [CStatement] [CStatement]

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

increment :: CDeclaration
increment = FunctionDeclaration "add" (NamedType "int")
               [("i", NamedType "int")]
               [ ReturnStatement
                   (InfixFn "+"
                       (CIdentifier "i")
                       (CInt 1)
                   )
               ]

enumDecl :: CDeclaration
enumDecl = EnumDeclaration "RGB" ["RED", "GREEN", "BLUE"]

mainDecl :: CDeclaration
mainDecl = FunctionDeclaration "main" (NamedType "int")
    [ ("i", (NamedType "int"))
    , ("args", CPointer (CPointer (NamedType "char")))]
    [ ExprAsStatment $
      FunctionCall "printf" [CText "%d", FunctionCall "fib" [CInt 5]]
    ]

fibSmart :: CDeclaration
fibSmart = FunctionDeclaration "fib" (NamedType "int")
  [("i", NamedType "int")]
  [ VariableInit "p1" (NamedType "int") (CInt 1)
  , VariableInit "p2" (NamedType "int") (CInt 1)
  , CWhile (InfixFn ">" (CIdentifier "i") (CInt 2))
    [ VariableAssign "i" (InfixFn "-" (CIdentifier "i") (CInt 1))
    , VariableInit "res" (NamedType "int") (InfixFn "+"
      (CIdentifier "p1")
      (CIdentifier "p2"))
    , VariableAssign "p1" (CIdentifier "p2")
    , VariableAssign "p2" (CIdentifier "res")
    ]
  , ReturnStatement (CIdentifier "p2")
  ]


fibonnacciStupid :: CDeclaration
fibonnacciStupid = FunctionDeclaration "stupidFib" (NamedType "int")
  [("i", NamedType "int")]
  [ IfStatement (InfixFn "<" (CIdentifier "i") (CInt 2))
    [ReturnStatement $ CInt 1]
    [ ReturnStatement $ InfixFn "+"
      (FunctionCall "stupidFib" [InfixFn "-" (CIdentifier "i") (CInt 1)])
      (FunctionCall "stupidFib" [InfixFn "-" (CIdentifier "i") (CInt 1)])
      ]
  ]

test :: CProgram
test = CProgram [ enumDecl
                , fibonnacciStupid
                , fibSmart
                , increment
                , mainDecl
                ]


