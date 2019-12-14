{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import AST
import Parser
import Lexer
import Text.Parsec (SourceName, ParseError, runParser)
import System.Environment
import Protolude

parseAll :: SourceName -> Text -> Either ParseError Program
parseAll name src = do tokens <- tokenize name src
                       parseProg name tokens

parseProg :: SourceName -> [Token] -> Either ParseError Program
parseProg = runParser parseProgram ()


main :: IO ()
main = do args <- getArgs
          case args of
               [file] -> do
                 content <- readFile file
                 print $ parseAll file content
               _ -> putStrLn "enter a file to parse"
