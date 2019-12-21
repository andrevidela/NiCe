{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module LexerSpec where

import Test.Hspec
import Parser
import Parser.Utils
import Lexer
import AST
import Text.Parsec
import Protolude

isSuccess :: Either a b -> Bool
isSuccess (Right _) = True
isSuccess _ = False

shouldSucceed :: (Show a, Show b) => (Either a b) -> Expectation
shouldSucceed a = shouldSatisfy a isSuccess


infixl 1 <|

(<|) :: a -> (a -> b) -> b
(<|) a f = f a

testLexer :: Text -> Either ParseError [Token]
testLexer = tokenize "test"

lexSpec :: Spec
lexSpec = do
  describe "general parsing" $ do
         it "should lex empty programs" $
           testLexer "" <| shouldSucceed
         it "should lex empty let" $
           testLexer "let a : type" <| shouldSucceed
         it "should lex lets with expressions" $
           testLexer "let a : type = 3" <| shouldSucceed
         it "should lex lets with infix expressions" $
           testLexer "let a: b = c * d"  <| shouldSucceed
         it "should lex function Expressions" $
           testLexer "let a: b = c()" <| shouldSucceed
         it "should lex nested function applications" $
           testLexer "let a : b = c()()" <| shouldSucceed
         it "should lex function definitions" $
           testLexer "let a : b = c d { }" <| shouldSucceed
         it "should lex struct definitions" $
           testLexer "struct List { let value1 : Int; }" <| shouldSucceed
  describe "functionParsing" $ do
      it "should lex simple function signatures" $
        testLexer "Int -> Int" <| shouldSucceed
      it "should lex function signatures with modified return type" $
        testLexer "Int -> ~Int" <| shouldSucceed
      it "should lex function signatures with modified types" $
        testLexer "~Int -> ~Int" <| shouldSucceed
      it "should lex complex function signatures" $
        testLexer "~>~Int, ~Int -> >(~Int, >Char -> >>Int)" <| shouldSucceed
      it "should lex nested function signatures" $
        testLexer "Int , (Int -> Int) -> Int" <| shouldSucceed
      it "should lex anonymous functions" $
        testLexer "a b { return c; }" <| shouldSucceed
      it "should lex the application of anonymous functions" $
        testLexer "a { return a; }(c)" <| shouldSucceed
  describe "statement parsing" $ do
      it "should lex while statements" $
        testLexer "while a == f(b) { return false; }" <| shouldSucceed
      it "should lex return statements" $
        testLexer "return \"kek\"" <| shouldSucceed
  describe "expression parsing" $ do
      it "should lex functions with wildcard args" $ do
        testLexer "_ acc { acc + 1; }" <| shouldSucceed
      it "should lex prefix expressions" $
        testLexer ">a" `shouldBe` Right [TPrefix ">", TIdent "a", EOF]
      it "should lex posfix expressions" $
        testLexer "a> " `shouldBe` Right [TIdent "a", TPostfix ">", EOF]
      it "should lex both pre and postfix at the same time" $
        testLexer " >a> " `shouldBe` Right [TPrefix ">", TIdent "a", TPostfix ">", EOF]
      it "should lex plain identifiers as expressions" $
        testLexer "a" `shouldBe` Right [TIdent "a", EOF]
      it "should lex lambdas" $
        testLexer "a b { return c; }" `shouldBe`
        (Right [TIdent "a", TIdent "b", LBrace, TReturn, TIdent "c", Semi, RBrace, EOF])
      it "should lex empty arugment list for lambda" $
        testLexer "{ return a; }" `shouldBe`
        (Right [LBrace, TReturn, TIdent "a", Semi, RBrace, EOF])
      it "should lex empty lambda declarations" $
        testLexer "let a : b = { }" `shouldBe`
          (Right [TLet, TIdent "a", Colon, TIdent "b", EqualSign, LBrace, RBrace, EOF])
      it "should lex complex lambda declarations" $
        testLexer "let a : b -> c = arg { }" <| shouldSucceed
      it "should lex simple lambda declarations" $
        testLexer "let a : b = { print(3); }" <| shouldSucceed
      it "should lex struct projection" $
        testLexer "a.b" `shouldBe` Right [TIdent "a", Dot, TIdent "b", EOF]
      it "should lex function application" $
        testLexer "f(a, b, c)" <| shouldSucceed
      it "should lex if expressions" $
        testLexer "if a then b else c" <| shouldSucceed
      it "should lex infix expressions" $
        testLexer "true != false" `shouldBe`
          Right [TTrue, TInfix "!=", TFalse, EOF]
      it "should lex infix expressions" $
        testLexer "true == false" <| shouldSucceed
  describe "others" $ do
      it "should ignore line comments" $
        testLexer "let a: b//test comment\nlet a: b" `shouldBe`
          (Right [TLet, TIdent "a", Colon, TIdent "b",TLet, TIdent "a", Colon, TIdent "b", EOF])
      it "should ignore line comments at end of program" $
        testLexer "let a: b//test comment" `shouldBe`
          (Right [TLet, TIdent "a", Colon, TIdent "b",EOF])

      it "should ignore multi line comments" $
        testLexer "let a: b/*test comment\nnext line comment */\nlet a: b" <| shouldSucceed
  describe "Whole programs" $ do
      it "should lex a whole program" $
        testLexer "\
        \struct List { let value: Int;                     \n\
        \              let tail: List;}                    \n\n\
        \let reduce : >List , ~Int -> (Int , Int -> Int) = \n\
        \  ls acc op {                                     \n\
        \    let curr : ~>List = acc;                      \n\
        \    while curr != NULL {                          \n\
        \      /*acc = op(ls>.value);                      \n\
        \      curr = curr>.tail; */                       \n\
        \    };                                            \n\
        \    return acc;                                   \n\
        \  }" <| shouldSucceed
      it "should lex multiArg lambda declarations" $
        testLexer "let fn = a b c { return a }" `shouldBe`
        (Right [TLet, TIdent "fn", EqualSign, TIdent "a", TIdent "b", TIdent "c", LBrace, TReturn, TIdent "a", RBrace, EOF])
      it "should lex complex lambda declarations" $
        testLexer "let accumulate : Unit = ls acc op { \n\
          \    let curr : ~>List = acc;   \n\
          \    while curr != NULL {       \n\
          \        acc = op(ls>.value);   \n\
          \        curr = curr>.tail;     \n\
          \    };                         \n\
          \}" <| shouldSucceed
      it "should lex complex signatures" $
        testLexer "let reduce : >List , ~Int -> (Int , Int -> Int)" <| shouldSucceed
