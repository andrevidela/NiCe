module Main where

import AST
import Parser
import Lexer
import Text.Parsec

parseAll :: SourceName -> String -> Either ParseError Program
parseAll name src = do tokens <- tokenize name src
                       parseProg name tokens

parseProg :: SourceName -> [Token] -> Either ParseError Program
parseProg name tokens = runParser parseProgram () name tokens

testParser :: Parser a -> String -> Either ParseError a
testParser p string = do tokens <- tokenize "test" string
                         runParser p () "test" tokens

main :: IO ()
main = putStrLn "topkek"
