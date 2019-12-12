{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.CCodegen where

import Backend.CAST
import Text.PrettyPrint.Leijen as T
  (Doc, comma, encloseSep, indent
  , lbrace, rbrace, linebreak, lparen
  , rparen, parens, semi, text, tupled
  , vsep,list, (<+>), (<>)
  )
import Backend.CTypes
import Protolude


generateType :: CType -> Doc
generateType = flip generateTypedIdentifier Nothing

textToDoc :: Text -> Doc
textToDoc = text . toS

ttrace :: Text -> a -> a
ttrace = trace

star :: Doc
star = text "*"

equal :: Doc
equal = text "="

cblock :: [Doc] -> Doc
cblock lines = lbrace <> linebreak <> indent 4 (vsep lines) <> linebreak <> rbrace

-- Given a C type and its identifier generate the
generateTypedIdentifier :: CType -> Maybe Text -> Doc
generateTypedIdentifier (NamedType name) (Just id) = textToDoc name <+> textToDoc id
generateTypedIdentifier (NamedType name) Nothing = textToDoc name

generateTypedIdentifier (CPointer tpe)   (Just id) = generateType tpe <> text "*" <+> textToDoc id
generateTypedIdentifier (CPointer tpe)   Nothing   = generateType tpe <+> text "*"

-- consts are ignored for now, everything is mutable
-- generateTypedIdentifier (CArray tpe Nothing) identifier = generateType tpe <+> text "*"
-- generateTypedIdentifier (CArray tpe (Just size)) identifier = generateType tpe <+> text "[" <> int size <> "]"
generateTypedIdentifier (CConst tpe)     id = generateType tpe
generateTypedIdentifier (CFuncPtr args ret) (Just identifier) =
    generateType ret <+> parens (star <> textToDoc identifier)
    <> (encloseSep lparen rparen comma $ map (generateType) args)
generateTypedIdentifier (CUnit) (Just identifier) = text "void" <+> text (toS identifier)
generateTypedIdentifier (CUnit) Nothing = text "void"

generateStructFields :: [(Text, CType)] -> Doc
generateStructFields fields = vsep $ map makeField fields
  where
    makeField :: (Text, CType) -> Doc
    makeField (name, tpe) = generateTypedIdentifier tpe (Just name) <+> text (toS name) <> text ";"

generateExpr :: CExpr -> Doc
generateExpr (CInt val) = text $ show val
generateExpr (CText val) = text $ show val
generateExpr (CChar val) = text $ show val
generateExpr (CDeref expr) = star <> parens (generateExpr expr)
generateExpr (FunctionCall f args) = textToDoc f <> tupled (map generateExpr args)
generateExpr (CIdentifier name) = textToDoc name
generateExpr (InfixFn op lhs rhs) = parens (
    generateExpr lhs
    <+> textToDoc op <+>
    generateExpr rhs)

generateStatement' :: CStatement -> Doc
generateStatement' (CWhile test body) = text "while" <> parens (generateExpr test) <+>
  cblock (generateStatement <$> body)
generateStatement' (VariableDecl name tpe) = generateTypedIdentifier tpe (Just name)
generateStatement' (VariableAssign name val) = textToDoc name <+> text "=" <+> generateExpr val
generateStatement' (VariableInit name tpe val) =
  generateTypedIdentifier tpe (Just name) <+> equal <+> generateExpr val
generateStatement' (ExprAsStatment expr) = generateExpr expr
generateStatement' (ReturnStatement expr) = text "return" <+> generateExpr expr
generateStatement' (IfStatement expr thenBody elseBody) =
  text "if" <> parens (generateExpr expr) <+> cblock
           (generateStatement <$> thenBody)
           <+> text "else" <+> cblock
           (generateStatement <$> elseBody)
generateStatement :: CStatement -> Doc
generateStatement = (<> semi) . generateStatement'

generateDecl :: CDeclaration -> Doc
generateDecl (StructDeclaration name fields) =
  text "struct" <+> text (toS name) <+> cblock [generateStructFields fields]
generateDecl (EnumDeclaration name values) =
  text "enum" <+> (encloseSep lbrace rbrace comma $ map textToDoc values)
generateDecl (GlobalVarDeclaration name tpe value) =
  generateTypedIdentifier tpe (Just name) <+> text "=" <+> text (toS name)
generateDecl (FunctionDeclaration name ret args body) =
  (generateType ret) <+> textToDoc name <> tupled (map (uncurry generateTypedIdentifier . swap . justFirst) args)
    <+> cblock (map generateStatement body)
    where
      justFirst :: (a, b) -> (Maybe a, b)
      justFirst (a, b) = (Just a, b)

generateProgram :: CProgram -> Doc
generateProgram (CProgram decls) = vsep $ map ((<> semi) . generateDecl) decls

