{-# LANGUAGE TypeApplications #-}

module TypeCheckerTests (tests) where

import           Common
import           Data.Text            (Text)
import           Test.HUnit           hiding (path)
import           Tiger.Expr           (Expr)
import           Tiger.Parser         (parse)
import           Tiger.Semant         (PosedSemantException (..), SemantException (..),
                                       Type (..), Unique (..), exceptionToText,
                                       posedExceptionToText, semantAnalyze)

import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text

import qualified Tiger.Amd64          as Amd64
import           Tiger.EscapeAnalysis (escapeAnalyze)

tests :: Test
tests = mkTestLabel "type checker tests"
    [ successFile "testcases/test1.tig"
    , successFile "testcases/test2.tig"
    , successFile "testcases/test3.tig"
    , successFile "testcases/test4.tig"
    , successFile "testcases/test5.tig"
    , successFile "testcases/test6.tig"
    , successFile "testcases/test7.tig"
    , successFile "testcases/test8.tig"
    , failureFile "testcases/test9.tig" (TypeMismatch tInt tString)
    , failureFile "testcases/test10.tig" (TypeMismatch tUnit tInt)
    , failureFile "testcases/test11.tig" (TypeMismatch tInt tString)
    , successFile "testcases/test12.tig"
    , failureFile "testcases/test13.tig" (TypeMismatch tInt tString)
    , failureFile "testcases/test13.tig" (TypeMismatch tInt tString)
    , failureFile "testcases/test14.tig" $
        TypeMismatch (tRecord "rectype" [("name", tString), ("id", tInt)])
                     (tArray tInt)
    , failureFile "testcases/test15.tig" (TypeMismatch tUnit tInt)
    , failureFile "testcases/test16.tig" (CycleTypeDec "d")
    , failureFile "testcases/test17.tig" (UndefinedType "treelist")
    , failureFile "testcases/test18.tig" (UndefinedFunction "do_nothing2")
    , failureFile "testcases/test19.tig" (UndefinedVariable "a")
    , failureFile "testcases/test20.tig" (UndefinedVariable "i")
    , failureFile "testcases/test21.tig" (TypeMismatch tInt tUnit)
    , failureFile "testcases/test22.tig" (RecordHasNoField "rec1" "nam")
    , failureFile "testcases/test23.tig" (TypeMismatch tString tInt)
    , failureFile "testcases/test24.tig" (NotArray "d")
    , failureFile "testcases/test25.tig" (NotRecord "d")
    , failureFile "testcases/test26.tig" (TypeMismatch tInt tString)
    , successFile "testcases/test27.tig"
    , failureFile "testcases/test28.tig" $
        TypeMismatch (tRecord "rectype1" [("name", tString), ("id", tInt)])
                     (tRecord "rectype2" [("name", tString), ("id", tInt)])
    , failureFile "testcases/test29.tig" (TypeMismatch (tArray tInt) (tArray tInt))
    , successFile "testcases/test30.tig"
    , failureFile "testcases/test31.tig" (TypeMismatch tInt tString)
    , failureFile "testcases/test32.tig" (TypeMismatch tInt tString)
    , failureFile "testcases/test33.tig" (UndefinedType "rectype")
    , failureFile "testcases/test34.tig" (TypeMismatch tInt tString)
    , failureFile "testcases/test35.tig" (ArgumentsNumberMismatch 2 1)
    , failureFile "testcases/test36.tig" (ArgumentsNumberMismatch 2 3)
    , successFile "testcases/test37.tig"
    , successFile "testcases/test38.tig"
    , successFile "testcases/test39.tig"
    , failureFile "testcases/test40.tig" (TypeMismatch tUnit tInt)
    , successFile "testcases/test41.tig"
    , successFile "testcases/test42.tig"
    , failureFile "testcases/test43.tig" UnitAssignment
    , successFile "testcases/test44.tig"
    , failureFile "testcases/test45.tig" NilAssignmentWithoutType
    , successFile "testcases/test46.tig"
    , successFile "testcases/test47.tig"
    , successFile "testcases/test48.tig"
    , failureFile "testcases/test50.tig" (DuplicatedRecordField "x")
    , failureFile "testcases/test51.tig" (DuplicatedRecordField "y")
    , failureFile "testcases/test52.tig" (UnitializedRecordField "rec" "y")
    , failureFile "testcases/test53.tig" $
        NotArrayType (tRecord "rec" [("x", tInt), ("y", tString)])
    , failureFile "testcases/test54.tig" BreakOutsideLoop
    , successFile "testcases/test55.tig"
    , failureFile "testcases/test56.tig" (TypeMismatch (tArray tInt) (tArray tInt))
    , successFile "testcases/test57.tig"
    , successFile "testcases/merge.tig"
    , successFile "testcases/queens.tig"
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

failureFile :: FilePath -> SemantException -> Assertion
failureFile path expErr = runSemantAnalyzer path >>= \case
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

successFile :: FilePath -> Assertion
successFile path = runSemantAnalyzer path >>= \case
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
            semantAnalyze @Amd64.Frame path escapeResult >>= \case
                Left err -> pure (Left err)
                Right _  -> pure (Right expr)
