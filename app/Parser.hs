module Parser where

import AST
import Lexer
import Data.List
import Data.Either
import Text.Parsec
import Text.Parsec.Prim
import Control.Monad
import Data.Functor.Identity
-- import Control.Applicative

-- General parsing tools
sat :: (Token -> Bool) -> Parser Token
sat f = tokenPrim showTok nextPos testTok
    where
      showTok t     = show t
      testTok t     = if f t then Just t else Nothing
      nextPos p t s = p

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither a b = (Left <$> try a) <|> (Right <$> b)

type Parser r = ParsecT [Token] () Maybe r

parseIdent :: Parser String
parseIdent = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe String
      testTok (TIdent str) = Just str
      testTok _ = Nothing
      nextPos p t s = p

-- Parse Type signatures
parseSimpleType :: Parser TypeDecl
parseSimpleType = SimpleType <$> parseIdent

parseFunctionType :: Parser TypeDecl
parseFunctionType = do head <- parseSimpleType
                       _ <- sat (==Comma)
                       rest <- parseSimpleType `sepBy` sat (==Comma)
                       _ <- sat (==RightArrow)
                       returnType <- parseSimpleType
                       return $ FunctionType head rest returnType
                     

parseTypeDecl :: Parser TypeDecl
parseTypeDecl = try parseFunctionType <|> parseSimpleType


-- parse expressions
parseExpr :: Parser Expr
parseExpr = undefined

-- parse Let 
parseEmptyLet :: Parser EmptyLet
parseEmptyLet = do _ <- sat (==TLet)
                   id <- parseIdent
                   _ <- sat (==Colon)
                   tpe <- parseTypeDecl
                   return $ EmptyLet id tpe

parseExprLet :: Parser ExprLet
parseExprLet = do _ <- sat (==TLet)
                  id <- parseIdent
                  _ <- sat (==Colon)
                  tpe <- parseTypeDecl
                  _ <- sat (==EqualSign)
                  expr <- parseExpr
                  return $ ExprLet id tpe expr

parseLetDecl :: Parser LetDecl
parseLetDecl = parseEither parseEmptyLet parseExprLet

parseLet :: Parser Definition
parseLet = LetDef <$> parseLetDecl 

-- parse Enums
parseEnumCase :: Parser String
parseEnumCase = sat (==Case) *> parseIdent

parseEnum :: Parser Definition
parseEnum = do _ <- sat (==TEnum)
               id <- parseIdent
               decls <- surroundBrace (many parseEnumCase)
               return $ (EnumDef id decls)

-- parse Structs
parseStruct :: Parser Definition
parseStruct = do _ <- sat (==Struct)
                 id <- parseIdent
                 decls <- surroundBrace $ many parseEmptyLet
                 return (StructDef id decls)

-- Surrounding parsers for ()[]{}
surroundBrack :: Parser p -> Parser p
surroundBrack p = do _ <- sat (==LBrack)
                     v <- p
                     _ <- sat (==RBrack)
                     return v

surroundParen :: Parser p -> Parser p
surroundParen p = do _ <- sat (==LParen)
                     v <- p
                     _ <- sat (==RParen)
                     return v

surroundBrace :: Parser p -> Parser p
surroundBrace p = do _ <- sat (==LBrace)
                     v <- p
                     _ <- sat (==RBrace)
                     return v

parseProgram :: Parser Program
parseProgram = many $ choice [parseLet, parseStruct, parseEnum]
