module Lexer where

import AbsGrammar
import Text.Parsec.Prim (runPT, runP, runParserT, runParser)
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Control.Monad (void)
import Data.Char (isLetter, isDigit, isSpace)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (token, tokens, runParser)
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Control.Monad ((>>=))

type TokenPos = (Token, SourcePos)


data Token = If
           | Then
           | Else
           | TLet
           | EqualSign
           | Whitespace
           | EOL
           | LParen
           | RParen
           | LBrack
           | RBrack
           | LBrace
           | RBrace
           | Struct
           | TEnum
           | Case
           | Semi
           | Colon
           | Dot
           | Comma
           | RightArrow
           | WavyMut
           | DoubleQuote
           | SingleQuote
           | TIdent String
           | Operator String
           | TPrefix String
           | TInfix String
           | TPostfix String
               deriving (Show, Eq)

parseOp :: Parser TokenPos
parseOp = parsePos $ do
                     ops <- many1 $ oneOf "&|*/-+<>%^=!¬∀∴~\\†∃∵∫√≤≥:•°·⊕§∷$"
                     return (Operator ops)
parseID :: Parser TokenPos
parseID = parsePos $ do
                     i <- firstChar 
                     n <- many nonFirstChar
                     return (TIdent (i : n))
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> char '\'' <|> firstChar

parsePos :: Parser Token -> Parser TokenPos
parsePos p = (,) <$> p <*> getPosition

eolToken :: Parser TokenPos
eolToken = parsePos $ many1 (char '\n') >> return EOL
whitespace :: Parser TokenPos
whitespace = parsePos $ (many1 space) >> return Whitespace

ifToken :: Parser TokenPos
ifToken = parsePos $ string "if" >> return If
thenToken :: Parser TokenPos
thenToken = parsePos $ string "then" >> return Then
elseToken :: Parser TokenPos
elseToken = parsePos $ string "else" >> return Else
letToken :: Parser TokenPos
letToken = parsePos $ string "let" >> return TLet
eqToken :: Parser TokenPos
eqToken = parsePos $ string "=" >> return EqualSign


lparen, rparen, lbrack, rbrack, lbrace, rbrace :: Parser TokenPos
lparen = parsePos $ char '(' >> return LParen
rparen = parsePos $ char ')' >> return RParen
lbrack = parsePos $ char '[' >> return LBrack
rbrack = parsePos $ char ']' >> return RBrack
lbrace = parsePos $ char '{' >> return LBrace
rbrace = parsePos $ char '}' >> return RBrace

structToken :: Parser TokenPos
structToken = parsePos $ string "struct" >> return Struct
enumToken :: Parser TokenPos
enumToken = parsePos $ string "enum" >> return TEnum
caseToken :: Parser TokenPos
caseToken = parsePos $ string "case" >> return Case
semiToken :: Parser TokenPos
semiToken = parsePos $ string ";" >> return Semi
colonToken :: Parser TokenPos
colonToken = parsePos $ string ":" >> return Colon
dotToken :: Parser TokenPos
dotToken = parsePos $ string "." >> return Dot
commaToken :: Parser TokenPos
commaToken = parsePos $ string "," >> return Comma
rightArrowToken :: Parser TokenPos
rightArrowToken = parsePos $ string "->" >> return RightArrow
wavyMutToken :: Parser TokenPos
wavyMutToken = parsePos $ string "~" >> return WavyMut
doubleQuoteToken :: Parser TokenPos
doubleQuoteToken = parsePos $ string "\"" >> return DoubleQuote
singleQuoteToken :: Parser TokenPos
singleQuoteToken = parsePos $ string "\'" >> return SingleQuote


parseSpace :: Parser TokenPos
parseSpace = parsePos $ (many1 space) *> return Whitespace

token :: Parser TokenPos
token = choice
    [ eolToken
    , parseSpace
    , try ifToken <|> parseID
    , try thenToken <|> parseID
    , try elseToken <|> enumToken <|> parseID
    , try letToken <|> parseID
    , eqToken
    , lbrack
    , rbrack
    , lbrace
    , rbrace
    , try structToken <|> parseID
    , semiToken
    , colonToken
    , dotToken
    , rightArrowToken
    , wavyMutToken
    , doubleQuoteToken
    , singleQuoteToken
    , try parseOp <|> parseID
    ]

tokens :: Parser [TokenPos]
tokens = many token

mapOperators :: [Token] -> [Token]
mapOperators (Whitespace : (Operator str) : Whitespace : rest) = (TInfix str) : (mapOperators $ Whitespace : rest)
mapOperators (Whitespace : (Operator str) : rest) = (TPostfix str) : (mapOperators $ Whitespace : rest)
mapOperators ((Operator str) : Whitespace : rest) = (TPrefix str) : (mapOperators rest)
mapOperators (x : xs) = x : (mapOperators xs)
mapOperators [] = []

removeWhitespace :: [Token] -> [Token]
removeWhitespace ls = filter (/=Whitespace) ls

padWhitespace :: [Token] -> [Token]
padWhitespace ls = (Whitespace : ls) ++ [Whitespace]
 

liftPair :: (a -> b) -> (a, c) -> (b, c)
liftPair fn (f, s) = (fn f, s)

infixl 8 |>
(|>) :: a -> (a -> b) -> b
(|>) a f = f a

helper :: Monad m => (m a -> m b) -> (a, c) -> m (b, c)
helper fn (f, s) = fmap (\b -> (b, s)) $  fn (return f)

liftPairM :: Monad m => (m a -> m b) -> m (a, c) -> m (b, c)
liftPairM fn pair = pair >>= (helper fn)

tokenize :: SourceName -> String -> Either ParseError [Token]
tokenize name text = do result <- parse name text 
                        let tokens = map fst result
                        return $ cleanup tokens
  where 
    parse = runParser tokens ()
    cleanup = removeWhitespace . mapOperators . padWhitespace
