{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser.Utils where

import Parser
import Lexer
import AST
import Protolude
import Text.Parsec
import GHC.Base (String)

parseProg :: SourceName -> [Token] -> Either ParseError Program
parseProg = runParser parseProgram ()

parseAll :: (StringConv s GHC.Base.String, IsString s) => SourceName -> s -> Either ParseError Program
parseAll name src = do tokens <- tokenize name src
                       parseProg name tokens
