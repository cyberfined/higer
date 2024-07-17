{-# LANGUAGE TypeApplications #-}

module TypeCheckerTests (tests) where

import           Data.Text            (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Common
import           Tiger.Expr           (Expr)
import           Tiger.Parser         (parse)
import           Tiger.Semant         (PosedSemantException (..), SemantException (..),
                                       Type (..), exceptionToText, posedExceptionToText,
                                       semantAnalyze)
import           Tiger.Temp           (InitLabel (..), InitTemp (..), runTempM)
import           Tiger.Unique         (Unique (..))

import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text

import qualified Tiger.Amd64          as Amd64
import           Tiger.EscapeAnalysis (escapeAnalyze)

tests :: TestTree
tests = testGroup "Type checker tests"
    [ successFile "type checks array creation" "testcases/test1.tig"
    , successFile "type checks array creation of type alias" "testcases/test2.tig"
    , successFile "type checks record creation" "testcases/test3.tig"
    , successFile "type checks recursive function" "testcases/test4.tig"
    , successFile "type checks program with recursive types" "testcases/test5.tig"
    , successFile "type checks mutually recursive procedures" "testcases/test6.tig"
    , successFile "type checks mutually recursive functions" "testcases/test7.tig"
    , successFile "type checks correct if" "testcases/test8.tig"
    , failureFile "failed to type check if with different return types"
        "testcases/test9.tig" (TypeMismatch tInt tString)
    , failureFile "failed to type check while with non-unit body"
        "testcases/test10.tig" (TypeMismatch tUnit tInt)
    , failureFile "failed to type check for with non-int hi expr"
        "testcases/test11.tig" (TypeMismatch tInt tString)
    , successFile "type checks for and let" "testcases/test12.tig"
    , failureFile "failed to type check int with string comparison"
        "testcases/test13.tig" (TypeMismatch tInt tString)
    , failureFile "failed to type check array with record comparison"
        "testcases/test14.tig" $
            TypeMismatch (tRecord "rectype" [("name", tString), ("id", tInt)])
                         (tArray tInt)
    , failureFile "failed to type check no-unit if without else"
        "testcases/test15.tig" (TypeMismatch tUnit tInt)
    , failureFile "failed to type check type synonyms with cycle"
        "testcases/test16.tig" (CycleTypeDec "d")
    , failureFile "failed to type check mutually recursive types defined in \
                  \different blocks" "testcases/test17.tig" (UndefinedType "treelist")
    , failureFile "failed to type check mutually recursive functions defined in \
                  \different blocks"
                  "testcases/test18.tig" (UndefinedFunction "do_nothing2")
    , failureFile "failed to type check undefined variable access in functions"
        "testcases/test19.tig" (UndefinedVariable "a")
    , failureFile "failed to type check undefined variable access in while"
        "testcases/test20.tig" (UndefinedVariable "i")
    , failureFile "failed to type check procedure returning value 1"
        "testcases/test21.tig" (TypeMismatch tInt tUnit)
    , failureFile "failed to type check undefined record field assignment"
        "testcases/test22.tig" (RecordHasNoField "rec1" "nam")
    , failureFile "failed to type check record fields assignment with type \
                  \mismtached values" "testcases/test23.tig" (TypeMismatch tString tInt)
    , failureFile "failed to type check non-array variable indexing"
        "testcases/test24.tig" (NotArray "d")
    , failureFile "failed to type check non-record variable dot access"
        "testcases/test25.tig" (NotRecord "d")
    , failureFile "failed to type check int and string addition"
        "testcases/test26.tig" (TypeMismatch tInt tString)
    , successFile "type checks function with parameter that shadows global"
        "testcases/test27.tig"
    , failureFile "failed to type check different record comparison"
        "testcases/test28.tig" $
            TypeMismatch (tRecord "rectype1" [("name", tString), ("id", tInt)])
                         (tRecord "rectype2" [("name", tString), ("id", tInt)])
    , failureFile "failed to type check different array comparison"
        "testcases/test29.tig" (TypeMismatch (tArray tInt) (tArray tInt))
    , successFile "type checks array creation with type synonym" "testcases/test30.tig"
    , failureFile "failed to type check string initialization of int annotated variable"
        "testcases/test31.tig" (TypeMismatch tInt tString)
    , failureFile "failed to type check string initialization of int array"
        "testcases/test32.tig" (TypeMismatch tInt tString)
    , failureFile "failed to type check undefined record creation"
        "testcases/test33.tig" (UndefinedType "rectype")
    , failureFile "failed to type check function call with wrong parameters types"
        "testcases/test34.tig" (TypeMismatch tInt tString)
    , failureFile "failed to type check function call with less parameters count"
        "testcases/test35.tig" (ArgumentsNumberMismatch 2 1)
    , failureFile "failed to type check function call with greater parameters count"
        "testcases/test36.tig" (ArgumentsNumberMismatch 2 3)
    , successFile "type checks variable redeclaration" "testcases/test37.tig"
    , successFile "type checks redeclaration" "testcases/test38.tig"
    , failureFile "failed to type check function redeclaration in the same block"
        "testcases/test39.tig" (DuplicatedFunctionDefinition "g")
    , failureFile "failed to type check procedure returning value 2"
        "testcases/test40.tig" (TypeMismatch tUnit tInt)
    , successFile "type checks local type redeclaration" "testcases/test41.tig"
    , successFile "type checks program with all supported definitions"
        "testcases/test42.tig"
    , failureFile "failed to type check unit assignment"
        "testcases/test43.tig" UnitAssignment
    , successFile "type check record nil initialization" "testcases/test44.tig"
    , failureFile "failed to type check nil initialization without type annotation"
        "testcases/test45.tig" NilAssignmentWithoutType
    , successFile "type checks record with nil comparison" "testcases/test46.tig"
    , successFile "type checks type redeclaration in different blocks"
        "testcases/test47.tig"
    , successFile "type checks function redeclaration in different blocks"
        "testcases/test48.tig"
    , failureFile "failed to type check record type with duplicated field"
        "testcases/test50.tig" (DuplicatedRecordField "x")
    , failureFile
        "failed to type check record creation with duplicated field initialization"
        "testcases/test51.tig" (DuplicatedRecordField "y")
    , failureFile "failed to type check record creation without field initialization"
        "testcases/test52.tig" (UnitializedRecordField "rec" "y")
    , failureFile "failed to type check array creation with record type"
        "testcases/test53.tig" $
            NotArrayType (tRecord "rec" [("x", tInt), ("y", tString)])
    , failureFile "failed to type check break outside the loop"
        "testcases/test54.tig" BreakOutsideLoop
    , successFile "type checks break inside the loop" "testcases/test55.tig"
    , failureFile "failed to type check different array types comparison"
        "testcases/test56.tig" (TypeMismatch (tArray tInt) (tArray tInt))
    , successFile "type checks same array types comparison" "testcases/test57.tig"
    , successFile "type checks merge sort implementation" "testcases/merge.tig"
    , successFile "type checks 8-queens peoblem solution" "testcases/queens.tig"
    , successFile "type checks atoi and itoa implementation" "testcases/atoi.tig"
    ]

tInt :: Type
tInt = TInt

tString :: Type
tString = TString

tRecord :: Text -> [(Text, Type)] -> Type
tRecord rec fs = TRecord rec fs (Unique 0)

tArray :: Type -> Type
tArray t = TArray t (Unique 0)

tUnit :: Type
tUnit = TUnit

failureFile :: TestName -> FilePath -> SemantException -> TestTree
failureFile name path expErr = testCase name $ do
    runSemantAnalyzer path >>= \case
        Left (PosedSemantException _ err _)
          | isErrorsMatch expErr err -> pure ()
          | otherwise -> assertFailure $  "type checking error mismatch: expecting "
                                       ++ Text.unpack (exceptionToText expErr)
                                       ++ ", actual "
                                       ++ Text.unpack (exceptionToText err)
        Right expr -> assertFailure $  "unexpected success when type checking `"
                                    ++ path
                                    ++ "`:\n"
                                    ++ show (EqExpr expr)
  where isErrorsMatch :: SemantException -> SemantException -> Bool
        isErrorsMatch (UndefinedVariable v1) (UndefinedVariable v2) = v1 == v2
        isErrorsMatch (UndefinedType t1) (UndefinedType t2) = t1 == t2
        isErrorsMatch (RecordHasNoField r1 f1) (RecordHasNoField r2 f2) =  r1 == r2
                                                                        && f1 == f2
        isErrorsMatch (NotRecord t1) (NotRecord t2) = t1 == t2
        isErrorsMatch (NotArray t1) (NotArray t2) = t1 == t2
        isErrorsMatch (TypeMismatch t11 t12) (TypeMismatch t21 t22)
          = isTypesMatch t11 t21 && isTypesMatch t12 t22
        isErrorsMatch (DuplicatedRecordField f1) (DuplicatedRecordField f2) = f1 == f2
        isErrorsMatch (CycleTypeDec t1) (CycleTypeDec t2) = t1 == t2
        isErrorsMatch EmptyName EmptyName = True
        isErrorsMatch (NotRecordType t1) (NotRecordType t2) = isTypesMatch t1 t2
        isErrorsMatch (UnitializedRecordField r1 f1) (UnitializedRecordField r2 f2)
          = r1 == r2 && f1 == f2
        isErrorsMatch (NotArrayType t1) (NotArrayType t2) = isTypesMatch t1 t2
        isErrorsMatch BreakOutsideLoop BreakOutsideLoop = True
        isErrorsMatch UnitAssignment UnitAssignment = True
        isErrorsMatch (UndefinedFunction f1) (UndefinedFunction f2) = f1 == f2
        isErrorsMatch (ArgumentsNumberMismatch e1 a1) (ArgumentsNumberMismatch e2 a2)
          = e1 == e2 && a1 == a2
        isErrorsMatch UnitComparison UnitComparison = True
        isErrorsMatch NilAssignmentWithoutType NilAssignmentWithoutType = True
        isErrorsMatch (DuplicatedFunctionDefinition f1) (DuplicatedFunctionDefinition f2)
          = f1 == f2
        isErrorsMatch _ _ = False

        isTypesMatch :: Type -> Type -> Bool
        isTypesMatch TInt TInt = True
        isTypesMatch TString TString = True
        isTypesMatch (TRecord r1 fs1 _) (TRecord r2 fs2 _) =
            let isFieldsMatch ((f1, t1):xs) ((f2, t2):ys)
                  | f1 == f2 && isTypesMatch t1 t2 = isFieldsMatch xs ys
                  | otherwise = False
                isFieldsMatch [] [] = True
                isFieldsMatch _ _ = False
            in r1 == r2 && isFieldsMatch fs1 fs2
        isTypesMatch (TArray t1 _) (TArray t2 _) = isTypesMatch t1 t2
        isTypesMatch TNil TNil = True
        isTypesMatch TUnit TUnit = True
        isTypesMatch _ _ = False

successFile :: TestName -> FilePath -> TestTree
successFile name path = testCase name $ do
    runSemantAnalyzer path >>= \case
        Left err -> assertFailure $  "unexpected type error `"
                                  ++ path
                                  ++ ":`\n"
                                  ++ Text.unpack (posedExceptionToText err)
        Right _ -> pure ()

runSemantAnalyzer :: FilePath -> IO (Either PosedSemantException Expr)
runSemantAnalyzer path = do
    src <- Text.readFile path
    case parse path src of
        Left err -> assertFailure $  "unexpected parsing error `"
                                  ++ path
                                  ++ ":`\n"
                                  ++ Text.unpack err
        Right expr -> do
            escapeResult <- escapeAnalyze expr
            runTempM (InitTemp 0) (InitLabel 0) $
                semantAnalyze @Amd64.Frame path escapeResult >>= \case
                    Left err -> pure (Left err)
                    Right _  -> pure (Right expr)
