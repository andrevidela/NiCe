module ParseSpec where

import Test.Hspec
import Parser
import AST
import Text.Parsec
import Main

testParseProgram :: String -> Either ParseError Program
testParseProgram = parseAll "test"

isSuccess :: Either a b -> Bool
isSuccess (Right _) = True
isSuccess _ = False

shouldSucceed a = shouldSatisfy a isSuccess

infixl 1 <|

(<|) :: a -> (a -> b) -> b
(<|) a f = f a

letSpec :: Spec
letSpec = do 
  describe "letparsing" $ do
         it "should parse empty programs" $ 
           testParseProgram "" `shouldBe` (Right [])
         it "should parse empty let" $
           testParseProgram "let a : type" `shouldBe` (Right [LetDef (Right (EmptyLet {emptyLetID = "a", emptyLetType = SimpleType "type"}))])
         it "should parse lets with expressions" $
           testParseProgram "let a : type = 3" `shouldBe` Right [LetDef (Left (ExprLet {exprLetID = "a", exprLetType = SimpleType "type", exprLetExpr = IntLit 3}))]
         it "should parse lets with infix expressions" $
           testParseProgram "let a: b = c * d"  <| shouldSucceed
         it "should parse function Expressions" $
           testParseProgram "let a: b = c()" <| shouldSucceed
         it "should parse nested function applications" $
           testParseProgram "let a : b = c()()" <| shouldSucceed
         it "should parse function definitions" $
           testParseProgram "let a : b = { }" <| shouldSucceed
  describe "functionParsing" $ do
      it "should parse anonymous functions" $
        testParser parseAnonFun "a b { return c; }" <| shouldSucceed
      it "should parse the application of anonymous functions" $
        testParser parseFapp "a { return a; }(c)" `shouldBe` Right (FApp (AnonFun ["a"] [Return (PlainIdent "a")]) [PlainIdent "c"])
