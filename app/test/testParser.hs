module ParseSpec where

import Test.Hspec
import Parser
import AST
import Text.Parsec
import Main

testParse :: String -> Either ParseError Program
testParse = parseAll "test"

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
           testParse "" `shouldBe` (Right [])
         it "should parse empty let" $
           testParse "let a : type" `shouldBe` (Right [LetDef (Right (EmptyLet {emptyLetID = "a", emptyLetType = SimpleType "type"}))])
         it "should parse lets with expressions" $
           testParse "let a : type = 3" `shouldBe` Right [LetDef (Left (ExprLet {exprLetID = "a", exprLetType = SimpleType "type", exprLetExpr = IntLit 3}))]
         it "should parse lets with infix expressions" $
           testParse "let a: b = c * d"  <| shouldSucceed
         it "should parse function Expressions" $
           testParse "let a: b = c()" <| shouldSucceed
         it "should parse nested function applications" $
           testParse "let a : b = c()()" <| shouldSucceed
