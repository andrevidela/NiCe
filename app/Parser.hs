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

postfixChain :: Parser a -> Parser (a -> a) -> Parser a
postfixChain p op = do
  x <- p
  rest x
  where
    rest x = (do f <- op
                 rest $ f x) <|> return x
postfixChain1 :: Parser a -> Parser (a -> a) -> Parser a
postfixChain1 p op = do
    x <- p
    f <- op
    rest (f x)
    where
      rest x = (do f <- op
                   rest $ f x) <|> return x

commaSeparated p = p `sepBy` (sat (==Comma))
parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither a b = (Left <$> try a) <|> (Right <$> b)

type Parser r = Parsec [Token] () r

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

parseMutableType :: Parser TypeDecl
parseMutableType = MutableType <$> (sat (==WavyMut) *>  parseTypeDeclNoFun)

parsePointerType :: Parser TypeDecl
parsePointerType = PointerType <$> ((sat (==(TPrefix ">")) <|> sat (==(Operator ">"))) *>
  parseTypeDeclNoFun)

parseFunctionType :: Parser TypeDecl
parseFunctionType = do head <- parseTypeDeclNoFun
                       args <- parseArgs
                       _ <- sat (==RightArrow)
                       returnType <- parseTypeDecl
                       return $ FunctionType head (args) returnType
  where
    parseArgs = many (sat (==Comma) *> parseTypeDeclNoFun)

parseTypeDeclNoFun :: Parser TypeDecl
parseTypeDeclNoFun = choice [ surroundParen parseTypeDecl
                            , parsePointerType
                            , parseMutableType
                            , parseSimpleType
                            ]

parseTypeDecl :: Parser TypeDecl
parseTypeDecl = choice [ surroundParen parseTypeDecl
                       , try parseFunctionType 
                       , parsePointerType
                       , parseMutableType
                       , parseSimpleType
                       ]


-- parse expressions
parseExpr :: Parser Expr
parseExpr = choice [ surroundParen parseExpr
                   , parseBoolLit
                   , parseStrLit
                   , try parseFloatLit <|> parseIntLit
                   , try parseInfix
                   , try parseFapp
                   , parsePrefix
                   , try parseAnonFun
                   , try parsePostfix
                   , parseIDExpr
                   ]

parseIDExpr :: Parser Expr
parseIDExpr = PlainIdent <$> parseIdent


parseFapp :: Parser Expr
parseFapp = postfixChain1 nonLeftRecExpr parseArgs
    where
      parseArgs :: Parser (Expr -> Expr)
      parseArgs = do args <- surroundParen (commaSeparated parseExpr)
                     return (\fn -> FApp fn args)

nonLeftRecExpr :: Parser Expr
nonLeftRecExpr = choice [ parsePrefix
                        , surroundParen parseExpr
                        , parseIntLit
                        , parseFloatLit
                        , parseStrLit
                        , try parseAnonFun
                        , parseIDExpr
                        ]

parseFloatLit :: Parser Expr
parseFloatLit = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe Expr
      testTok (TFloatLit flt) = Just (FloatLit flt)
      testTok _ = Nothing
      nextPos p t s = p

parseIntLit :: Parser Expr
parseIntLit = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe Expr
      testTok (TIntLit int) = Just (IntLit int)
      testTok _ = Nothing
      nextPos p t s = p

parseBoolLit :: Parser Expr
parseBoolLit = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe Expr
      testTok (TTrue) = Just (BoolLit True)
      testTok (TFalse) = Just (BoolLit False)
      testTok _ = Nothing
      nextPos p t s = p

parseStrLit :: Parser Expr
parseStrLit = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe Expr
      testTok (StringLit str) = Just (StrLit str)
      testTok _ = Nothing
      nextPos p t s = p

parseAnonFun :: Parser Expr
parseAnonFun = do args <- many parseIdent
                  stmts <- surroundBrace $ many parseStatement
                  return $ AnonFun args stmts

parseInfix :: Parser Expr
parseInfix = do lhs <- parseExprNoInfix
                op <- parseOpInfix
                rhs <- parseExpr
                return $ InfixOp op lhs rhs
  where
    parseExprNoInfix = choice [ -- try parseFapp
                                parsePrefix
                              , parsePostfix
                              , parseIntLit
                              , parseBoolLit
                              , parseStrLit
                              --, try parseAnonFun
                              , parseIDExpr
                              ]

parsePrefix :: Parser Expr
parsePrefix = do op <- parseOpPrefix
                 e <- parseExpr
                 return $ PrefixOp op e

parsePostfix :: Parser Expr
parsePostfix = postfixChain1 nonLeftRecExpr post
    where
      post = do op <- parseOpPostfix
                return (\e -> PostfixOp e op)
  
parseOpInfix :: Parser String
parseOpInfix = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe String
      testTok (TInfix str) = Just str
      testTok _ = Nothing
      nextPos p t s = p
parseOpPostfix :: Parser String
parseOpPostfix = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe String
      testTok (TPostfix str) = Just str
      testTok _ = Nothing
      nextPos p t s = p
parseOpPrefix :: Parser String
parseOpPrefix = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe String
      testTok (TPrefix str) = Just str
      testTok _ = Nothing
      nextPos p t s = p

-- parse statements
parseStatement :: Parser Statement
parseStatement = choice [ SLet <$> parseLetDecl
                        , parseWhile
                        , parseReturn
                        , parsePlain
                        ] <* sat (==Semi)

parseWhile :: Parser Statement
parseWhile = do sat (==TWhile)
                cond <- parseExpr
                stmts <- surroundBrace (many parseStatement)
                return $ While cond stmts

parseReturn :: Parser Statement
parseReturn = Return <$> (sat (==TReturn) *> parseExpr)

parsePlain :: Parser Statement
parsePlain = Plain <$> parseExpr

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
parseLetDecl = parseEither parseExprLet parseEmptyLet

parseLet :: Parser Definition
parseLet = LetDef <$> parseLetDecl 

-- parse Enums
parseEnumCase :: Parser String
parseEnumCase = sat (==Case) *> parseIdent <* sat (==Semi)

parseEnum :: Parser Definition
parseEnum = do _ <- sat (==TEnum)
               id <- parseIdent
               decls <- surroundBrace (many parseEnumCase)
               return $ (EnumDef id decls)

-- parse Structs
parseStruct :: Parser Definition
parseStruct = do _ <- sat (==Struct)
                 id <- parseIdent
                 decls <- surroundBrace $ many (parseEmptyLet <* sat (==Semi))
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
surroundBrace p = do sat (==LBrace)
                     v <- p
                     sat (==RBrace)
                     return v

parseProgram :: Parser Program
parseProgram = (many $ choice [parseLet, parseStruct, parseEnum]) <* sat (==EOF)

