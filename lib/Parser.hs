{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
module Parser where

import           AST
import           Control.Monad
import           Data.Either
import           Data.Functor.Identity
import           Data.List
import           Data.List.NonEmpty
import           Lexer
import           Protolude             hiding (many, try, (<|>))
import           Text.Parsec           (Parsec, ParsecT, SourcePos, Stream,
                                        choice, many, many1, parse, satisfy,
                                        sepBy, tokenPrim, try, (<|>))

-- General parsing tools
sat :: (Token -> Bool) -> Parser Token
sat f = tokenPrim show nextPos testTok
    where
      testTok t     = if f t then Just t else Nothing

nextPos :: SourcePos -> Token -> [Token] -> SourcePos
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
                             parseHelp x f
                    where
                      rest x = do { f <- op
                                  ; parseHelp x f
                                  } <|> return x
                      parseHelp x f = do y <- p
                                         rest (f x y)

commaSeparated :: ParsecT [Token] () Identity a -> ParsecT [Token] () Identity [a]
commaSeparated = (`sepBy` sat (==Comma))

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither a b = (Left <$> try a) <|> (Right <$> b)

type Parser r = Parsec [Token] () r

parseIdent :: Parser Text
parseIdent = tokenPrim show nextPos testTok
    where
      testTok :: Token -> Maybe Text
      testTok (TIdent str) = Just str
      testTok _            = Nothing

-- Parse Type signatures
parseSimpleType :: Parser TypeDecl
parseSimpleType = SimpleType <$> parseIdent

data TypePrefix = Mutable | Pointer deriving (Show, Eq)
parseTypePrefix :: Parsec Text () [TypePrefix]
parseTypePrefix = many1 singlePrefix

singlePrefix :: Parsec Text () TypePrefix
singlePrefix = mutablePrefix <|> pointerPrefix

mutablePrefix :: Parsec Text () TypePrefix
mutablePrefix = satisfy (=='~') >> return Mutable

pointerPrefix :: Parsec Text () TypePrefix
pointerPrefix = satisfy (=='>')
                >> return Pointer

combinePrefix :: [TypePrefix] -> TypeDecl -> TypeDecl
combinePrefix (Mutable : rest) tpe = MutableType (combinePrefix rest tpe)
combinePrefix (Pointer : rest) tpe = PointerType (combinePrefix rest tpe)
combinePrefix [] tpe               = tpe

magicParsePrefix :: Parser [TypePrefix]
magicParsePrefix = tokenPrim show nextPos parseTok
    where
      parseTok :: Token -> Maybe [TypePrefix]
      parseTok (TPrefix str) = case parse parseTypePrefix "TypePrefixParsing" str of
                                    Right ts -> Just ts
                                    Left _   -> Nothing
      parseTok _ = Nothing

parseModifiedType :: Parser TypeDecl
parseModifiedType = do prefix <- magicParsePrefix
                       combinePrefix prefix <$> parseTypeDeclNoFun


parseFunctionType :: Parser TypeDecl
parseFunctionType = do head <- parseTypeDeclNoFun
                       args <- parseArgs
                       _ <- sat (==RightArrow)
                       FunctionType (head :| args) <$> parseTypeDecl
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
                     return (flip FApp args)

parseProjection :: Parser Expr
parseProjection = postfixChain1 nonLeftRecExpr parseProj
    where
      parseProj :: Parser (Expr -> Expr)
      parseProj = sat (==Dot) >> flip Proj <$> parseIdent

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
parseFloatLit = tokenPrim show nextPos testTok
    where
      testTok :: Token -> Maybe Expr
      testTok (TFloatLit flt) = Just (FloatLit flt)
      testTok _               = Nothing

parseIntLit :: Parser Expr
parseIntLit = tokenPrim show nextPos testTok
    where
      testTok :: Token -> Maybe Expr
      testTok (TIntLit int) = Just (IntLit int)
      testTok _             = Nothing

parseBoolLit :: Parser Expr
parseBoolLit = tokenPrim show nextPos testTok
    where
      testTok :: Token -> Maybe Expr
      testTok TTrue  = Just (BoolLit True)
      testTok TFalse = Just (BoolLit False)
      testTok _      = Nothing

parseStrLit :: Parser Expr
parseStrLit = tokenPrim show nextPos testTok
    where
      testTok :: Token -> Maybe Expr
      testTok (StringLit str) = Just (StrLit str)
      testTok _               = Nothing

parseIfExpr :: Parser Expr
parseIfExpr = do _ <-sat (==If)
                 cond <- parseExpr
                 _ <-sat (==Then)
                 t <- parseExpr
                 _ <-sat (==Else)
                 IfExpr cond t <$> parseExpr

parseAnonFun :: Parser Expr
parseAnonFun = do args <- many parseFArg
                  stmts <- surroundBrace $ many parseStatement
                  return $ AnonFun args stmts
parseFArg :: Parser FArg
parseFArg = (FArgument <$> parseIdent) <|> (sat (==Wildcard) >> return WildCardArg)

parseInfix :: Parser Expr
parseInfix = chainl2 nonLeftRecExpr parseExpr binaryOp
    where
      binaryOp :: Parser (Expr -> Expr -> Expr)
      binaryOp = InfixOp <$> parseOpInfix

parsePrefix :: Parser Expr
parsePrefix = do op <- parseOpPrefix
                 PrefixOp op <$> parseExpr

parsePostfix :: Parser Expr
parsePostfix = postfixChain1 nonLeftRecExpr post
    where
      post = flip PostfixOp <$> parseOpPostfix

parseOpInfix :: Parser Text
parseOpInfix = tokenPrim show nextPos testTok
    where
      testTok :: Token -> Maybe Text
      testTok (TInfix str) = Just str
      testTok _            = Nothing

parseOpPostfix :: Parser Text
parseOpPostfix = tokenPrim show nextPos testTok
    where
      testTok :: Token -> Maybe Text
      testTok (TPostfix str) = Just str
      testTok _              = Nothing

parseOpPrefix :: Parser Text
parseOpPrefix = tokenPrim show nextPos testTok
    where
      testTok :: Token -> Maybe Text
      testTok (TPrefix str) = Just str
      testTok _             = Nothing

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
                      _ <- sat (==EqualSign)
                      Assign i <$> parseExpr

parseIfStmt :: Parser Statement
parseIfStmt = do _ <- sat (==If)
                 cond <- parseExpr
                 t <- surroundBrace (many parseStatement)
                 _ <- sat (==Else)
                 e <- surroundBrace (many parseStatement)
                 return $ IfStmt cond t e

parseWhile :: Parser Statement
parseWhile = do _ <- sat (==TWhile)
                cond <- parseExpr
                stmts <- surroundBrace (many parseStatement)
                return $ While cond stmts

parseReturn :: Parser Statement
parseReturn = Return <$> (sat (==TReturn) *> parseExpr)

parsePlain :: Parser Statement
parsePlain = Plain <$> parseExpr

-- parse Let
parseLetIdTpe :: Parser (Text, TypeDecl)
parseLetIdTpe = do _  <- sat (==TLet)
                   id <- parseIdent
                   _  <- sat (==Colon)
                   (id, ) <$> parseTypeDecl

parseEmptyLet :: Parser EmptyLet
parseEmptyLet = uncurry EmptyLet <$> parseLetIdTpe

parseExprLet :: Parser ExprLet
parseExprLet = do (id, tpe) <- parseLetIdTpe
                  _ <- sat (==EqualSign)
                  ExprLet id tpe <$> parseExpr

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
               EnumDef id <$> surroundBrace (many parseEnumCase)

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
surroundBrace p = do _ <- sat (==LBrace)
                     v <- p
                     _ <- sat (==RBrace)
                     return v

parseProgram :: Parser Program
parseProgram = many (choice [parseLet, parseStruct, parseEnum]) <* sat (==EOF)

