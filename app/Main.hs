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

type TokenPos = (Token, SourcePos)


data Token = If
           | Then
           | Else
           | Let
           | EqualSign
           | Whitespace
           | LParen
           | RParen
           | LBrack
           | RBrack
           | LBrace
           | RBrace
           | Struct
           | Enum
           | Semi
           | Colon
           | Dot
           | RightArrow
           | WavyMut
           | DoubleQuote
           | SingleQuote
           | Identifier String
           | Operator String
               deriving (Show, Eq)

parseOp :: Parser TokenPos
parseOp = parsePos $ do
                     ops <- many1 $ oneOf "&|*/-+<>%^=!¬∀∴~\\†∃∵∫√≤≥:•°·⊕§∷$"
                     return (Operator ops)
parseID :: Parser TokenPos
parseID = parsePos $ do
                     i <- firstChar 
                     n <- many nonFirstChar
                     return (Identifier (i : n))
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> char '\'' <|> firstChar

parsePos :: Parser Token -> Parser TokenPos
parsePos p = (,) <$> p <*> getPosition

whitespace :: Parser TokenPos
whitespace = parsePos $ (many1 space) >> return Whitespace

ifToken :: Parser TokenPos
ifToken = parsePos $ string "if" >> return If
thenToken :: Parser TokenPos
thenToken = parsePos $ string "then" >> return Then
elseToken :: Parser TokenPos
elseToken = parsePos $ string "else" >> return Else
letToken :: Parser TokenPos
letToken = parsePos $ string "let" >> return Let
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
enumToken = parsePos $ string "enum" >> return Enum
semiToken :: Parser TokenPos
semiToken = parsePos $ string ";" >> return Semi
colonToken :: Parser TokenPos
colonToken = parsePos $ string ":" >> return Colon
dotToken :: Parser TokenPos
dotToken = parsePos $ string "." >> return Dot
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
    [ parseSpace
    , ifToken
    , thenToken
    , try elseToken <|> enumToken
    , letToken
    , eqToken
    , lbrack
    , rbrack
    , lbrace
    , rbrace
    , structToken
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

tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser tokens ()

main :: IO ()
main = putStrLn "hello parser"
