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
  describe "general parsing" $ do
         it "should parse empty programs" $
           testParseProgram "" `shouldBe` (Right [])
         it "should parse empty let" $
           testParseProgram "let a : type" `shouldBe`
             (Right [LetDef (Right (EmptyLet {emptyLetID = "a", emptyLetType = SimpleType "type"}))])
         it "should parse lets with expressions" $
           testParseProgram "let a : type = 3" `shouldBe`
             Right [LetDef (Left (ExprLet { exprLetID = "a"
                                          , exprLetType = SimpleType "type"
                                          , exprLetExpr = IntLit 3}))]
         it "should parse lets with infix expressions" $
           testParseProgram "let a: b = c * d"  <| shouldSucceed
         it "should parse function Expressions" $
           testParseProgram "let a: b = c()" <| shouldSucceed
         it "should parse nested function applications" $
           testParseProgram "let a : b = c()()" <| shouldSucceed
         it "should parse function definitions" $
           testParseProgram "let a : b = c d { }" <| shouldSucceed
         it "should parse struct definitions" $
           testParseProgram "struct List { let value1 : Int; }" `shouldBe`
             Right [StructDef "List" [EmptyLet {emptyLetID = "value1", emptyLetType = SimpleType "Int"}]]
  describe "functionParsing" $ do
      it "should parse simple function signatures" $
        testParser parseTypeDecl "Int -> Int" `shouldBe` 
          Right (FunctionType (SimpleType "Int") [] (SimpleType "Int"))
      it "should parse function signatures with modified return type" $
        testParser parseTypeDecl "Int -> ~Int" `shouldBe` 
          Right (FunctionType (SimpleType "Int") [] (MutableType (SimpleType "Int")))
      it "should parse function signatures with modified types" $
        testParser parseTypeDecl "~Int -> ~Int" `shouldBe` 
          Right (FunctionType (MutableType (SimpleType "Int")) [] (MutableType (SimpleType "Int")))
      it "should parse complex function signatures" $
        testParser parseTypeDecl "~>~Int, ~Int -> >(~Int, >Char -> >>Int)" `shouldBe`
          Right (FunctionType (MutableType (PointerType (MutableType (SimpleType "Int"))))
                [MutableType (SimpleType "Int")]
                (PointerType (FunctionType (MutableType (SimpleType "Int"))
                                           [PointerType (SimpleType "Char")]
                                           (PointerType (PointerType (SimpleType "Int")))))
                )
      it "should parse nested function signatures" $
        testParser parseTypeDecl "Int , (Int -> Int) -> Int" `shouldBe`
          Right (FunctionType (SimpleType "Int") 
                              [FunctionType (SimpleType "Int") [] (SimpleType "Int")]
                              (SimpleType "Int"))
      it "should parse anonymous functions" $
        testParser parseAnonFun "a b { return c; }" <| shouldSucceed
      it "should parse the application of anonymous functions" $
        testParser parseFapp "a { return a; }(c)" `shouldBe`
          Right (FApp (AnonFun [FArgument "a"] [Return (PlainIdent "a")]) [PlainIdent "c"])
  describe "statement parsing" $ do
      it "should parse while statements" $
        testParser parseWhile "while a == f(b) { return false; }" <| shouldSucceed
      it "shoud parse return statements" $
        testParser parseReturn "return \"kek\"" <| shouldSucceed
  describe "expression parsing" $ do
      it "should parse functions with wildcard args" $ do 
        testParser parseExpr "_ acc { acc + 1; }" `shouldBe` 
          Right (AnonFun [WildCardArg, FArgument "acc"] [Plain (InfixOp "+" (PlainIdent "acc") (IntLit 1)) ])
      it "should parse prefix expressions" $
        testParser parseExpr ">a" `shouldBe` Right (PrefixOp ">" (PlainIdent "a"))
      it "should parse posfix expressions" $
        testParser parseExpr "a> " `shouldBe` Right (PostfixOp (PlainIdent "a") ">")
      it "should parse both pre and postfix at the same time" $
        testParser parseExpr " >a> " `shouldBe` Right (PrefixOp ">" (PostfixOp (PlainIdent "a") ">"))
      it "should parse plain identifiers as expressions" $
        testParser parseExpr "a" `shouldBe` Right (PlainIdent "a")
      it "should parse lambdas" $
        testParser parseExpr "a b { return c; }" `shouldBe` Right (AnonFun [FArgument "a", FArgument "b"] [Return (PlainIdent "c")])
      it "should parse empty arugment list for lambda" $
        testParser parseExpr "{ return a; }" `shouldBe` Right (AnonFun [] [Return (PlainIdent "a")])
      it "should parse empty lambda declarations" $
        testParseProgram "let a : b = { }" `shouldBe`
          Right [LetDef (Left (ExprLet { exprLetID = "a"
                                       , exprLetType = SimpleType "b"
                                       , exprLetExpr = (AnonFun [] [])}))]
      it "should parse simple lambda declarations" $
        testParseProgram "let a : b = { print(3); }" `shouldBe`
          Right [LetDef (Left (ExprLet { exprLetID = "a"
                                       , exprLetType = SimpleType "b"
                                       , exprLetExpr = (AnonFun []
                                                                [Plain (FApp (PlainIdent "print")
                                                                [IntLit 3])])}))]
      it "should parse struct projection" $
        testParser parseExpr "a.b" `shouldBe` Right (Proj (PlainIdent "a") "b")
      it "should parse function application" $
        testParser parseExpr "f(a, b, c)" `shouldBe`
          Right (FApp (PlainIdent "f") [PlainIdent "a", PlainIdent "b", PlainIdent "c"])
      it "should parse if expressions" $
        testParser parseExpr "if a then b else c" `shouldBe`
          Right (IfExpr (PlainIdent "a") (PlainIdent "b") (PlainIdent "c"))
      it "should parse infix expressions" $
        testParser parseExpr "true != false" `shouldBe` 
          Right (InfixOp "!=" (BoolLit True) (BoolLit False))
      it "should parse infix expressions" $
        testParser parseExpr "true == false" `shouldBe` 
          Right (InfixOp "==" (BoolLit True) (BoolLit False))
  describe "statement parsing" $ do
      it "should parse assignments" $
        testParser parseStatement "a = 3;" `shouldBe` Right (Assign "a" (IntLit 3))
      it "should parse if-statements" $
        testParser parseIfStmt "if true { a(); } else { a(); }" `shouldBe`
          Right (IfStmt (BoolLit True) [Plain (FApp (PlainIdent "a") [])] [Plain (FApp (PlainIdent "a") [])])
      it "should parse lambda with mutliple arguments" $
        testParser parseExpr "a b { }" `shouldBe` Right (AnonFun [FArgument "a", FArgument "b"]
                                                                 [])
      it "should parse lambdas with  if statements" $
        testParser parseExpr  "args { if true { print(b);} else { print(b); }; }" `shouldBe`
          Right (AnonFun [FArgument "args"]
                         [IfStmt (BoolLit True)
                                 [Plain $ FApp (PlainIdent "print") [PlainIdent "b"]]
                                 [Plain $ FApp (PlainIdent "print") [PlainIdent "b"]]])
  describe "others" $ do
      it "should ignore line comments" $ 
        testParseProgram "let a: b//test comment\nlet a: b" `shouldBe`
          (Right [ LetDef (Right (EmptyLet {emptyLetID = "a", emptyLetType = SimpleType "b"})) 
                 , LetDef (Right (EmptyLet {emptyLetID = "a", emptyLetType = SimpleType "b"}))])

      it "should ignore line comments at end of program" $ 
        testParseProgram "let a: b//test comment" `shouldBe` 
          (Right [LetDef (Right (EmptyLet {emptyLetID = "a", emptyLetType = SimpleType "b"}))])

      it "should ignore multi line comments" $ 
        testParseProgram "let a: b/*test comment\nnext line comment */\nlet a: b" `shouldBe`
          (Right [ LetDef (Right (EmptyLet {emptyLetID = "a", emptyLetType = SimpleType "b"})) 
                 , LetDef (Right (EmptyLet {emptyLetID = "a", emptyLetType = SimpleType "b"}))])
  describe "Whole programs" $ do
      it "should parse a whole program" $
        testParseProgram "\
        \struct List { let value: Int;\n\
        \              let tail: List; }\n\n\
        \let reduce : >List , ~Int -> (Int , Int -> Int) = \n\
        \  ls acc op { \n\
        \    let curr : ~>List = acc; \n\
        \    while curr /= NULL { \n\
        \      /*acc = op(ls>.value); \n\
        \      curr = curr>.tail; */\n\
        \    }; \n\
        \    return acc; \n\
        \  }" <| shouldSucceed
      it "should parse complex signatures" $
        testParseProgram "let reduce : >List , ~Int -> (Int , Int -> Int)" `shouldBe`
          Right [LetDef (Right (
            EmptyLet { emptyLetID = "reduce"
                     , emptyLetType = FunctionType (PointerType (SimpleType "List")) 
                                                   [MutableType (SimpleType "Int")]
                                                   (FunctionType (SimpleType "Int") 
                                                                 [SimpleType "Int"] 
                                                                 (SimpleType "Int"))}))]
