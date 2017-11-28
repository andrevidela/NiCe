module Typecheck where

import AST
import Data.Map
import ErrM

type Env = (Sig, [Context])
type Sig = Map String ([TypeDecl], TypeDecl)
type Context = Map String TypeDecl

lookupVar :: Env -> String -> Err TypeDecl
lookupVar = undefined


checkStmt :: Env -> TypeDecl -> Statement -> Err Env
checkStmt = undefined

checkDefinition :: Env -> TypeDecl -> Definition -> Err Env
checkDefinition env val (LetDef decl) = 
checkDefinition env val (StructDef _ decls) = 
checkDefinition env val (EnumDef name fields) = return env
