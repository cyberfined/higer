module Codegen.Tests (tests) where

import           Control.Monad      (forM_)
import           Data.Proxy         (Proxy (..))
import           Data.Typeable      (Typeable)
import           Test.Tasty
import           Test.Tasty.HUnit

import           BackendTestCases   (Input (..), TestCase (..))
import           Common             (genericCodegen, runInterpreter)
import           Tiger.Codegen      (Instruction, TempReg (..))
import           Tiger.Frame        (Instr, Reg)
import           Tiger.IR           (ControlFlowGraph)
import           Tiger.RegMachine   (FrameEmulator, FrameRegister (..), Interpretable,
                                     InterpreterResult (..), ReturnRegister (..))
import           Tiger.TextUtils    (TextBuildable (..))

import qualified Data.Text          as Text

import qualified BackendTestCases   as Backend
import qualified Codegen.Amd64Tests as Amd64Tests
import qualified Tiger.Amd64        as Amd64

tests :: TestTree
tests = testGroup "Codegen tests"
    [ amd64Tests
    ]

amd64Tests :: TestTree
amd64Tests = testGroup "AMD64 tests"
    [ runTestCases "semantic tests"
                   (Proxy @Amd64.LinuxFrame)
                   (Proxy @Amd64.TempRegEmulator)
                   (ReturnRegister (Reg Amd64.Rax))
                   (FrameRegister (Reg Amd64.Rbp))
                   allCases
    , Amd64Tests.genStmtTests
    , Amd64Tests.testRvFpAbsent (Proxy @Amd64.LinuxFrame) allCases
    ]
  where allCases = Backend.testCases ++ Amd64Tests.semanticTestCases

runTestCases :: forall f e. ( Typeable (Reg f)
                            , Enum (Reg f)
                            , Bounded (Reg f)
                            , TextBuildable (Reg f)
                            , Typeable (Instr f)
                            , TextBuildable (Instr f (TempReg (Reg f)))
                            , FrameEmulator f e (TempReg (Reg f)) (Instr f (TempReg (Reg f)))
                            , Interpretable f e (TempReg (Reg f))
                                                (Instr f (TempReg (Reg f)))
                                                (ControlFlowGraph (Instr f (TempReg (Reg f))))
                            , Instruction (Instr f) (TempReg (Reg f))
                            , Instruction (Instr f) (Reg f)
                            )
             => TestName
             -> Proxy f
             -> Proxy e
             -> ReturnRegister (TempReg (Reg f))
             -> FrameRegister (TempReg (Reg f))
             -> [TestCase]
             -> TestTree
runTestCases name framePrx emuPrx rv fp testCases = testGroup name
                                                  $ map runTestCase testCases
  where runTestCase :: TestCase -> TestTree
        runTestCase TestCase{..} = testCase testName $ do
            irData <- genericCodegen testSrc framePrx pure
            forM_ testInputs $ \Input{..} -> do
                res <- runInterpreter rv fp emuPrx irData inputStdin
                let errText = "when interpret \n" ++ Text.unpack testSrc ++ "\n\n"
                            ++ "input: "
                            ++ Text.unpack inputStdin
                assertEqual errText
                            (inputStdout, inputExitCode)
                            (resOutput res, resCode res)
