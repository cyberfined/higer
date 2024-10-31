module IRGenTests (tests) where

import           Control.Monad    (forM_)
import           Data.List        (find)
import           Data.Maybe       (fromJust)
import           Data.Proxy       (Proxy (..))
import           Data.Text        (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

import           BackendTestCases (Input (..), TestCase (..), itoaSrc, testCases)
import           Common           (genericCompileToIR, runInterpreter)
import           Tiger.Frame      (Access (..), Frame (..))
import           Tiger.IR         (IRData (..), IRFunction (..), Stmt)
import           Tiger.RegMachine (FrameEmulator, FrameRegister (..),
                                   InterpreterResult (..), ReturnRegister (..))
import           Tiger.Temp       (Label (..), Temp)

import qualified Data.Text        as Text

import qualified Tiger.Amd64      as Amd64
import qualified Tiger.Temp       as Temp

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
    (Proxy @Amd64.LinuxFrame) (Proxy @Amd64.IREmulator)

amd64SpecificTests :: TestTree
amd64SpecificTests = testGroup "specific tests"
    [ irTest @Amd64.LinuxFrame "locals and parameters frame offsets"
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
                isInFrames = [ Just 16 -- Static link
                             , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing
                             , Just 24 -- h
                             , Just 32 -- g
                             ]

        findFuncByPrefix prefix IRFunction{..} = case frameName irFuncFrame of
            LabelInt{}     -> False
            LabelText name -> Text.isPrefixOf prefix name

irTest :: forall f. Frame f
       => TestName
       -> [Text]
       -> [IRData Stmt f -> Assertion]
       -> TestTree
irTest name srcLines cases = testCase name $ do
    ir <- compileToIR (Text.unlines srcLines) (Proxy @f)
    mapM_ (\f -> f ir) cases

runTestCases :: forall f e. FrameEmulator f e Temp Stmt
             => TestName
             -> Proxy f
             -> Proxy e
             -> TestTree
runTestCases name framePrx Proxy = testGroup name $ map runTestCase testCases
  where runTestCase :: TestCase -> TestTree
        runTestCase TestCase{..} = testCase testName $ do
            irData <- compileToIR testSrc framePrx
            forM_ testInputs $ \Input{..} -> do
                let rv = ReturnRegister Temp.RV
                let fp = FrameRegister Temp.FP
                res <- runInterpreter rv fp (Proxy @e) irData inputStdin
                let errText =  "when interpret \n" ++ Text.unpack testSrc ++ "\n\n"
                            ++ "input: "
                            ++ Text.unpack inputStdin
                assertEqual errText
                            (inputStdout, inputExitCode)
                            (resOutput res, resCode res)

compileToIR :: forall f. Frame f => Text -> Proxy f -> IO (IRData Stmt f)
compileToIR src prxy = genericCompileToIR src prxy pure
