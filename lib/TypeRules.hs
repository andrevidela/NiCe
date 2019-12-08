module TypeRules where


data TypeDef = NamedType String
             | Alias String TypeDef
             | Unit
             | Void
             | Func TypeDef TypeDef
             -- built-in and literals
             | BoolType
             | IntType
             | StringType
             | CharType
             -- Pointer types
             | PointerType TypeDef
             | MutableType TypeDef

  deriving (Eq, Show)

data TypeError = Mismatch TypeDef TypeDef
               | Ambiguous [TypeDef]
               | ArgumentError [TypeDef] [TypeDef]

data TypedExpr = Explicit TypeDef
               | Inferred TypeDef
               | Candidates [TypeDef]

checkType :: TypedExpr -> TypedExpr -> Maybe TypeError
checkType (Explicit l) (Explicit r)       | l == r         = Nothing
                                          | otherwise      = Just (Mismatch l r)
checkType (Explicit l) (Inferred r)       | l == r         = Nothing
                                          | otherwise      = Just (Mismatch l r)
checkType (Inferred l) (Explicit r)       | l == r         = Nothing
                                          | otherwise      = Just (Mismatch l r)
checkType (Inferred l) (Inferred r)       | l == r         = Nothing
                                          | otherwise      = Just (Mismatch l r)
checkType (Candidates types) (Explicit r) | r `elem` types = Nothing
                                          | otherwise      = Just (Ambiguous types)
checkType (Candidates types) (Inferred r) | r `elem` types = Nothing
                                          | otherwise      = Just (Ambiguous types)
checkType (Explicit l) (Candidates types) | l `elem` types = Nothing
                                          | otherwise      = Just (Ambiguous types)
checkType (Inferred l) (Candidates types) | l `elem` types = Nothing
                                          | otherwise      = Just (Ambiguous types)
checkType (Candidates l) (Candidates r) = Just (Ambiguous $ l ++ r)

typecheckIf :: TypedExpr -> TypedExpr -> TypedExpr -> TypedExpr -> Maybe TypeError
typecheckIf cond ifTrue ifFalse expectedType = do _ <- checkType cond (Explicit BoolType)
                                                  _ <- checkType ifTrue expectedType
                                                  checkType ifFalse expectedType

typecheckLetExplicit :: TypedExpr -> TypedExpr -> Maybe TypeError
typecheckLetExplicit expected expression = checkType expression expected

typeCheckWhile :: TypedExpr -> TypedExpr -> Maybe TypeError
typeCheckWhile cond body = do _ <- checkType cond (Explicit BoolType)
                              checkType body (Explicit Unit)

-- typeCheckFApp :: TypedExpr -> [TypedExpr] -> TypedExpr -> [TypedExpr] -> Maybe TypeError
-- typeCheckFApp expectedReturn expectedArgs returnType givenArgs = do
--   _ <- checkType returnType expectedReturn
--   checkPairwise givenArgs expectedArgs
--   where
--     checkPairwise :: [TypedExpr] -> [TypedExpr] -> Maybe TypeError
--     checkPairwise [] [] = Nothing
--     checkPairwise (x : xs) [] = Just (ArgumentError givenArgs expectedArgs)
--     checkPairwise [] (x : xs) = Just (ArgumentError givenArgs expectedArgs)
--     checkPairwise (x : xs) (y : ys) = do _ <- checkType x y
--                                          checkPairwise xs ys
