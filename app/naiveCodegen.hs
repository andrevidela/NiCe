module NaiveCodegen where

import AST
import ErrM
import Data.Char

generateProgram :: Program -> Err String
generateProgram declarations = do generated <- (traverse generateDef declarations)
                                  return $ unlines generated

generateDef :: Definition -> Err String
generateDef (LetDef decl) = undefined
generateDef (EnumDef name elements) = undefined
generateDef (StructDef name fields) = return $ "struct " ++ name ++ "{" ++ generateFields fields ++ "}"
  where
    generateFields :: [EmptyLet] -> String
    generateFields fs = unlines $ (map ((\x -> x ++ ";\n") . generateEmptyLet) fs)


generateEmptyLet :: EmptyLet -> String
generateEmptyLet EmptyLet {emptyLetID=name, emptyLetType=tpe} = generateTypeDecl tpe ++ name

generateTypeDecl :: TypeDecl -> String
generateTypeDecl tpe = undefined
