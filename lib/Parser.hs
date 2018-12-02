{-# LANGUAGE NoImplicitPrelude #-}
module Parser where

import AST
import Lexer
import Data.List
import Data.Either
import Text.Parsec
--import Text.Parsec.Prim
import Control.Monad
import Data.Functor.Identity
import Protolude hiding (many, try, (<|>))

-- General parsing tools
sat :: (Token -> Bool) -> Parser Token
sat f = tokenPrim show nextPos testTok
    where
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

chainl2 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainl2 notLeftRec p op = do x <- notLeftRec
                             f <- op
                             y <- p
                             rest (f x y)
                    where
                      rest x    = do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    } <|> return x
commaSeparated = (`sepBy` sat (==Comma))
parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither a b = (Left <$> try a) <|> (Right <$> b)

type Parser r = Parsec [Token] () r

parseIdent :: Parser Text
parseIdent = tokenPrim show nextPos testTok
    where
      testTok :: Token -> Maybe Text
      testTok (TIdent str) = Just str
      testTok _ = Nothing
      nextPos p t s = p

-- Parse Type signatures
parseSimpleType :: Parser TypeDecl
parseSimpleType = SimpleType <$> parseIdent

data TypePrefix = Mutable | Pointer deriving (Show, Eq)
parseTypePrefix :: Parsec Text () [TypePrefix]
parseTypePrefix = many1 singlePrefix

singlePrefix :: Parsec Text () TypePrefix
singlePrefix = mutablePrefix <|> pointerPrefix

mutablePrefix :: Parsec Text () TypePrefix
mutablePrefix = do satisfy (=='~'); return Mutable

pointerPrefix :: Parsec Text () TypePrefix
pointerPrefix = do satisfy (=='>'); return Pointer

combinePrefix :: [TypePrefix] -> TypeDecl -> TypeDecl
combinePrefix (Mutable : rest) tpe = MutableType (combinePrefix rest tpe)
combinePrefix (Pointer : rest) tpe = PointerType (combinePrefix rest tpe)
combinePrefix [] tpe = tpe

magicParsePrefix :: Parser [TypePrefix]
magicParsePrefix = tokenPrim show nextPos parseTok
    where
      parseTok :: Token -> Maybe [TypePrefix]
      parseTok (TPrefix str) = case parse parseTypePrefix "TypePrefixParsing" str of
                                    Right ts -> Just ts
                                    Left _ -> Nothing
      parseTok _ = Nothing
      nextPos p t s = p

parseModifiedType :: Parser TypeDecl
parseModifiedType = do prefix <- magicParsePrefix
                       typeValue <- parseTypeDeclNoFun
                       return $ combinePrefix prefix typeValue


parseFunctionType :: Parser TypeDecl
parseFunctionType = do head <- parseTypeDeclNoFun
                       args <- parseArgs
                       _ <- sat (==RightArrow)
                       returnType <- parseTypeDecl
                       return $ FunctionType head args returnType
  where
    parseArgs = many (sat (==Comma) *> parseTypeDeclNoFun)

parseTypeDeclNoFun :: Parser TypeDecl
parseTypeDeclNoFun = choice [ surroundParen parseTypeDecl
                            , parseModifiedType
                            , parseSimpleType
                            ]

parseTypeDecl :: Parser TypeDecl
parseTypeDecl = choice [ try parseFunctionType 
                       , surroundParen parseTypeDecl
                       , parseModifiedType
                       , parseSimpleType
                       ]


-- parse expressions
parseExpr :: Parser Expr
parseExpr = choice [ surroundParen parseExpr
                   , try parseInfix
                   , try parseFloatLit <|> parseIntLit
                   , try parseProjection
                   , try parseFapp
                   , parsePrefix
                   , parseIfExpr
                   , try parseAnonFun
                   , try parsePostfix
                   , parseStrLit
                   , parseBoolLit
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

parseProjection :: Parser Expr
parseProjection = postfixChain1 nonLeftRecExpr parseProj
    where
      parseProj :: Parser (Expr -> Expr)
      parseProj  = do sat (==Dot)
                      id <- parseIdent 
                      return (\struct -> Proj struct id)

nonLeftRecExpr :: Parser Expr
nonLeftRecExpr = choice [ parsePrefix
                        , surroundParen parseExpr
                        , parseBoolLit
                        , parseIntLit
                        , parseIfExpr
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

parseIfExpr :: Parser Expr
parseIfExpr = do sat (==If)
                 cond <- parseExpr
                 sat (==Then)
                 t <- parseExpr
                 sat (==Else)
                 e <- parseExpr
                 return $ IfExpr cond t e

parseAnonFun :: Parser Expr
parseAnonFun = do args <- many parseFArg
                  stmts <- surroundBrace $ many parseStatement
                  return $ AnonFun args stmts
parseFArg :: Parser FArg
parseFArg = (FArgument <$> parseIdent) <|> (sat (==Wildcard) >> return WildCardArg)

parseInfix :: Parser Expr
parseInfix = do chainl2 nonLeftRecExpr parseExpr binaryOp
    where
      binaryOp :: Parser (Expr -> Expr -> Expr)
      binaryOp = do op <- parseOpInfix
                    return $ InfixOp op

parsePrefix :: Parser Expr
parsePrefix = do op <- parseOpPrefix
                 e <- parseExpr
                 return $ PrefixOp op e

parsePostfix :: Parser Expr
parsePostfix = postfixChain1 nonLeftRecExpr post
    where
      post = do op <- parseOpPostfix
                return (flip PostfixOp op)
  
parseOpInfix :: Parser Text
parseOpInfix = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe Text
      testTok (TInfix str) = Just str
      testTok _ = Nothing
      nextPos p t s = p
parseOpPostfix :: Parser Text
parseOpPostfix = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe Text
      testTok (TPostfix str) = Just str
      testTok _ = Nothing
      nextPos p t s = p
parseOpPrefix :: Parser Text
parseOpPrefix = tokenPrim (show) nextPos testTok
    where
      testTok :: Token -> Maybe Text
      testTok (TPrefix str) = Just str
      testTok _ = Nothing
      nextPos p t s = p

-- parse statements
parseStatement :: Parser Statement
parseStatement = choice [ SLet <$> parseLetDecl
                        , parseWhile
                        , parseReturn
                        , try parseIfStmt
                        , try parseAssignement
                        , parsePlain
                        ] <* sat (==Semi)

parseAssignement :: Parser Statement
parseAssignement = do i <- parseIdent
                      sat (==EqualSign)
                      e <- parseExpr
                      return $ Assign i e

parseIfStmt :: Parser Statement
parseIfStmt = do sat (==If)
                 cond <- parseExpr
                 t <- surroundBrace (many parseStatement)
                 sat (==Else)
                 e <- surroundBrace (many parseStatement)
                 return $ IfStmt cond t e 

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
parseEnumCase :: Parser Text
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

