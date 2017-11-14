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
import Text.Parsec.Number
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Control.Monad ((>>=))

type TokenPos = (Token, SourcePos)


data Token = If
           | Then
           | Else
           | TLet
           | TReturn
           | TWhile
           | EqualSign
           | TTrue
           | TFalse
           | Whitespace
           | EOL
           | EOF
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
           | TIntLit Int
           | TFloatLit Float
           | StringLit String
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

-- literals

parsePos :: Parser Token -> Parser TokenPos
parsePos p = (,) <$> p <*> getPosition

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseString :: Parser TokenPos
parseString = parsePos $ do char '"'
                            strings <- many character
                            char '"'
                            return $ StringLit (concat strings)
      
parseInteger :: Parser TokenPos
parseInteger = parsePos $ TIntLit <$> nat
-- Whitespace
eofToken = parsePos $ eof >> return EOF
eolToken :: Parser TokenPos
eolToken = parsePos $ many1 (char '\n') >> return EOL
whitespace :: Parser TokenPos
whitespace = parsePos $ (many1 space) >> return Whitespace

-- reserved
trueToken :: Parser TokenPos
trueToken = parsePos $ string "true" >> return TTrue
falseToken :: Parser TokenPos
falseToken = parsePos $ string "false" >> return TFalse
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
returnToken :: Parser TokenPos
returnToken = parsePos $ string "return" >> return TReturn
whileToken :: Parser TokenPos
whileToken = parsePos $ string "while" >> return TWhile

-- delimiters
lparen, rparen, lbrack, rbrack, lbrace, rbrace :: Parser TokenPos
lparen = parsePos $ char '(' >> return LParen
rparen = parsePos $ char ')' >> return RParen
lbrack = parsePos $ char '[' >> return LBrack
rbrack = parsePos $ char ']' >> return RBrack
lbrace = parsePos $ char '{' >> return LBrace
rbrace = parsePos $ char '}' >> return RBrace

-- symbols
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
    , try ifToken
    , try thenToken <|> try trueToken
    , try elseToken <|> enumToken
    , try letToken
    , try falseToken
    , try caseToken
    , parseInteger
    , eqToken
    , lbrack
    , rbrack
    , lbrace
    , rbrace
    , lparen
    , rparen
    , try structToken
    , semiToken
    , colonToken
    , dotToken
    , commaToken
    , rightArrowToken
    , wavyMutToken
    , parseString
    , singleQuoteToken
    , try parseOp <|> parseID
    ]

tokens :: Parser [TokenPos]
tokens = do tkns <- (many token)
            eof <- eofToken
            return $ tkns ++ [eof]

mapOperators :: [Token] -> [Token]
mapOperators (Whitespace : (Operator str) : Whitespace : rest) = (TInfix str) : (mapOperators $ Whitespace : rest)
mapOperators (Whitespace : (Operator str) : rest) = (TPrefix str) : (mapOperators $ Whitespace : rest)
mapOperators ((Operator str) : Whitespace : rest) = (TPostfix str) : (mapOperators rest)
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
    parse = runParser (tokens ) ()
    cleanup = removeWhitespace . mapOperators . padWhitespace
