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

sat :: (Token -> Bool) -> Parser Token
sat f = tokenPrim showTok nextPos testTok
    where
      showTok t     = show t
      testTok t     = if f t then Just t else Nothing
      nextPos p t s = p

parseIdent :: Parser String
parseIdent = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe String
      testTok (TIdent str) = Just str
      testTok _ = Nothing
      nextPos p t s = p

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither a b = (Left <$> try a) <|> (Right <$> b)

type Parser r = ParsecT [Token] () Maybe r

parseEmptyLet :: Parser EmptyLet
parseEmptyLet = do _ <- sat (==TLet)
                   id <- parseIdent
                   _ <- sat (==Colon)
                   tpe <- parseTypeDecl
                   return $ EmptyLet id tpe

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

parseExprLet :: Parser ExprLet
parseExprLet = do _ <- sat (==TLet)
                  id <- parseIdent
                  _ <- sat (==Colon)
                  tpe <- parseTypeDecl
                  _ <- sat (==EqualSign)
                  expr <- parseExpr
                  return $ ExprLet id tpe expr

parseExpr :: Parser Expr
parseExpr = undefined

parseLetDecl :: Parser LetDecl
parseLetDecl = parseEither parseEmptyLet parseExprLet

parseLet :: Parser Definition
parseLet = LetDef <$> parseLetDecl 

parseStruct :: Parser Definition
parseStruct = do id <- parseIdent
                 decls <- surroundBrace $ many parseEmptyLet
                 return (StructDef id decls)

parseEnum :: Parser Definition
parseEnum = undefined

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
