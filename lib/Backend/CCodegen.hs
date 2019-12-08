{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.CCodegen where

import Backend.CAST
import Text.PrettyPrint.Leijen (Doc, comma, encloseSep, indent, lbrace, rbrace, linebreak, lparen, rparen, parens, semi, text, tupled, vsep, (<+>), (<>))
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

sem :: Doc
sem = text ";"

equal :: Doc
equal = text "="

cblock :: [Doc] -> Doc
cblock lines = lbrace <> linebreak <> indent 4 (vsep lines) <> linebreak <> rbrace

-- Given a C type and its identifier generate the
generateTypedIdentifier :: CType -> Maybe Text -> Doc
generateTypedIdentifier (NamedType name) (Just id) = textToDoc name <+> textToDoc id
generateTypedIdentifier (NamedType name) Nothing = textToDoc name

generateTypedIdentifier (CPointer tpe)   (Just id) = generateType tpe <+> text "*" <+> textToDoc id

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

generateStatement :: CStatement -> Doc
generateStatement (CWhile test body) = text "while" <> parens (generateExpr test) <+>
  cblock ((<> sem) . generateStatement <$> body)
generateStatement (VariableDecl name tpe) = generateTypedIdentifier tpe (Just name)
generateStatement (VariableAssign name val) = textToDoc name <+> text "=" <+> generateExpr val
generateStatement (ExprAsStatment expr) = generateExpr expr



generateDecl :: CDeclaration -> Doc
generateDecl (StructDeclaration name fields) =
  text "struct" <+> text (toS name) <+> cblock [generateStructFields fields]
generateDecl (EnumDeclaration name values) = undefined
generateDecl (GlobalVarDeclaration name tpe value) =
  generateTypedIdentifier tpe (Just name) <+> text "=" <+> text (toS name) <> semi
generateDecl (FunctionDeclaration name ret args body) =
  (generateType ret) <+> textToDoc name <> tupled (map (uncurry generateTypedIdentifier . swap . justFirst) args)
    <+> cblock (map ((<> sem) . generateStatement) body)
    where
      justFirst :: (a, b) -> (Maybe a, b)
      justFirst (a, b) = (Just a, b)

generateProgram :: CProgram -> Doc
generateProgram (CProgram decls) = vsep $ map (generateDecl) decls

