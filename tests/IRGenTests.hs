{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IRGenTests (tests) where

import           Control.Monad        (forM_)
import           Data.List            (find)
import           Data.Maybe           (fromJust)
import           Data.Proxy           (Proxy (..))
import           Data.Text            (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

import           BackendTestCases     (Input (..), TestCase (..), itoaSrc, testCases)
import           Tiger.EscapeAnalysis (escapeAnalyze)
import           Tiger.Frame          (Access (..), Frame (..))
import           Tiger.IR             (FrameEmulator, IRData (..), IRDataStmt,
                                       IRFunction (..), InterpreterResult (..),
                                       newEmulator)
import           Tiger.Parser         (parse)
import           Tiger.Semant         (posedExceptionToText, semantAnalyze)
import           Tiger.Temp           (InitLabel (..), InitTemp (..), Label (..),
                                       runTempM)

import qualified Data.Text            as Text
import qualified Data.Text.Lazy       as LazyText

import qualified Tiger.Amd64          as Amd64
import qualified Tiger.IR             as IR

tests :: TestTree
tests = testGroup "IR generation tests"
    [ amd64Tests
    ]

amd64Tests :: TestTree
amd64Tests = testGroup "AMD64 tests"
    [ amd64GenericTests
    , amd64SpecificTests
    ]

amd64GenericTests :: TestTree
amd64GenericTests = runTestCases "generic tests"
    (Proxy @Amd64.Frame) (Proxy @Amd64.Emulator)

amd64SpecificTests :: TestTree
amd64SpecificTests = testGroup "specific tests"
    [ irTest @Amd64.Frame "locals and parameters frame offsets"
        [ "let"
        , itoaSrc
        , "  function test("
        , "    a: int, b: int, c: int, d: int, e: int, f: int,"
        , "    g: int, h: int"
        , "  ): int ="
        , "    let"
        , "      var v1 := 10"
        , "      var v2 := 11"
        , "      var v3 := 12"
        , "      function fun1(v3: int) = v1 := v2 + v3"
        , "    in"
        , "      fun1(v3);"
        , "      a + b + c + d + e + f + g + h"
        , "    end"
        , "in print(itoa(test(1, 2, 3, 4, 5, 6, 7, 8)))"
        , "end"
        ]
        [ testArgs
        ]
    ]
  where testArgs IRData{..} = do
            assertEqual "Arguments count mismatch" (length isInFrames) (length args)
            forM_ (zip isInFrames args) $ \case
                (Nothing, InFrame{}) ->
                    assertFailure "argument should be placed in register"
                (Just{}, InReg{}) ->
                    assertFailure "argument should be placed in frame"
                (Just expOff, InFrame actOff) ->
                    assertEqual "wrong argument frame offset" expOff actOff
                _ -> pure ()
          where IRFunction{..} = fromJust $ find (findFuncByPrefix "test") irFunctions
                args = frameArgs irFuncFrame
                isInFrames = [ Just 32 -- Static link
                             , Just 24 -- h
                             , Just 16 -- g
                             , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing
                             ]

        findFuncByPrefix prefix IRFunction{..} = case frameName irFuncFrame of
            LabelInt{}     -> False
            LabelText name -> Text.isPrefixOf prefix name

irTest :: forall f. Frame f
       => TestName
       -> [Text]
       -> [IRDataStmt f -> Assertion]
       -> TestTree
irTest name srcLines cases = testCase name $ do
    ir <- compileToIR (Text.unlines srcLines) (Proxy @f)
    mapM_ (\f -> f ir) cases

runTestCases :: forall f e. FrameEmulator f e => TestName -> Proxy f -> Proxy e -> TestTree
runTestCases name framePrx Proxy = testGroup name $ map runTestCase testCases
  where runTestCase :: TestCase -> TestTree
        runTestCase TestCase{..} = testCase testName $ do
            irData <- compileToIR testSrc framePrx
            forM_ testInputs $ \Input{..} -> do
                emu <- newEmulator @e framePrx
                res <- IR.runInterpreter emu irData inputStdin
                case resError res of
                    Just err -> assertFailure $  "unexpected IR interpreter error \n"
                                              ++ "input: "
                                              ++ Text.unpack inputStdin
                                              ++ "\noutput: "
                                              ++ LazyText.unpack (resOutput res)
                                              ++ "\nError: "
                                              ++ show err
                    Nothing -> do
                        let errText = "when interpret \n" ++ Text.unpack testSrc ++ "\n\n"
                                    ++ "input: "
                                    ++ Text.unpack inputStdin
                        assertEqual errText
                                    (inputStdout, inputExitCode)
                                    (resOutput res, resCode res)

compileToIR :: forall f. Frame f => Text -> Proxy f -> IO (IRDataStmt f)
compileToIR src Proxy = case parse "test.tig" src of
    Left err -> assertFailure $ "unexpected parsing error `" ++ Text.unpack err
    Right expr -> do
        escResult <- escapeAnalyze expr
        res <- runTempM (InitTemp 0) (InitLabel 0) $ semantAnalyze @f "test.tig" escResult
        case res of
            Left err -> assertFailure $  "unexpected type error `"
                                      ++ Text.unpack (posedExceptionToText err)
            Right ir -> pure ir
