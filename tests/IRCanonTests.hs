{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IRCanonTests (tests) where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

import           BackendTestCases       (Input (..), TestCase (..), testCases)
import           Data.List              (find)
import           Tiger.EscapeAnalysis   (escapeAnalyze)
import           Tiger.Frame            (Frame (..))
import           Tiger.IR               (Block (..), ControlFlowGraph (..), Expr (..),
                                         FrameEmulator, IRData (..), IRDataCFG,
                                         IRFunction (..), IRFunctionCFG,
                                         InterpreterResult (..), Stmt (..), canonicalize,
                                         newEmulator)
import           Tiger.Parser           (parse)
import           Tiger.Semant           (posedExceptionToText, semantAnalyze)
import           Tiger.Temp             (InitLabel (..), InitTemp (..), runTempM)

import qualified Data.Graph.Inductive   as Graph
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LazyText
import qualified Data.Text.Lazy.Builder as Builder

import qualified Tiger.Amd64            as Amd64
import qualified Tiger.Frame            as Frame
import qualified Tiger.IR               as IR
import qualified Tiger.Temp             as Temp

tests :: TestTree
tests = testGroup "IR canonicalization tests"
    [ amd64Tests
    ]

amd64Tests :: TestTree
amd64Tests = testGroup "AMD64 tests"
    [ runTestCases "semantic tests" (Proxy @Amd64.Frame) (Proxy @Amd64.Emulator)
    , runLinearizationTestCases "linearization tests" (Proxy @Amd64.Frame)
    ]

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

compileToIR :: forall f. Frame f => Text -> Proxy f -> IO (IRDataCFG f)
compileToIR src Proxy = case parse "test.tig" src of
    Left err -> assertFailure $ "unexpected parsing error `" ++ Text.unpack err
    Right expr -> do
        escResult <- escapeAnalyze expr
        runTempM (InitTemp 0) (InitLabel 0) $
            semantAnalyze @f "test.tig" escResult >>= \case
                Left err -> liftIO $
                    assertFailure $  "unexpected type error `"
                                  ++ Text.unpack (posedExceptionToText err)
                Right irStmt -> canonicalize irStmt

runLinearizationTestCases :: forall f. Frame f => TestName -> Proxy f -> TestTree
runLinearizationTestCases name framePrx = testGroup name $ map runTestCase testCases
  where runTestCase :: TestCase -> TestTree
        runTestCase TestCase{..} = testCase testName $ do
            IRData{..} <- compileToIR testSrc framePrx
            let nonLinearFunc = find (not . isLinearizedFunc) irFunctions
            case nonLinearFunc of
                Nothing   -> pure ()
                Just func -> assertFailure $  "Function "
                                           ++ showFuncName func
                                           ++ " is not linearized"

        isLinearizedFunc :: IRFunctionCFG f -> Bool
        isLinearizedFunc IRFunction{..} =
            all (blockIsCanon . fstBlockWithLabel funName . snd) blocks &&
            checkCJumpFallback irFuncBody
          where funName = Frame.frameName irFuncFrame
                blocks = Graph.labNodes $ cfgGraph irFuncBody

        fstBlockWithLabel :: Temp.Label -> Block -> Block
        fstBlockWithLabel funName b@Block{..}
          | blockLabel == funName = b { blockStmts = Label funName : blockStmts }
          | otherwise             = b

        blockIsCanon :: Block -> Bool
        blockIsCanon Block{..} = checks blockStmts
          where checks ss =  blockNonEmpty ss
                          && all isLinearizedStmt ss
                          && blockHasOnlyOneLabel ss
                          && blockHasOnlyOneJump ss

        isLinearizedStmt :: Stmt -> Bool
        isLinearizedStmt = \case
            Move e1 e2        -> isLinearizedExpr False e1 && isLinearizedExpr False e2
            Expr e            -> isLinearizedExpr False e
            Jump _            -> True
            CJump _ e1 e2 _ _ -> isLinearizedExpr False e1 && isLinearizedExpr False e2
            Seq _             -> False
            Label _           -> True
            Ret               -> True

        isLinearizedExpr :: Bool -> Expr -> Bool
        isLinearizedExpr isInCall = \case
            Const _       -> True
            Name _        -> True
            Temp _        -> True
            Binop _ e1 e2 -> isLinearizedExpr isInCall e1 && isLinearizedExpr isInCall e2
            Mem e         -> isLinearizedExpr isInCall e
            Call _ es     -> not isInCall && all (isLinearizedExpr True) es
            ESeq _ _      -> False

        blockHasOnlyOneLabel :: [Stmt] -> Bool
        blockHasOnlyOneLabel (Label _:ss) = blockHasOnlyOneLabel' ss
          where blockHasOnlyOneLabel' (Label _:_) = False
                blockHasOnlyOneLabel' (_:xs)      = blockHasOnlyOneLabel' xs
                blockHasOnlyOneLabel' _           = True
        blockHasOnlyOneLabel _ = False

        blockNonEmpty :: [Stmt] -> Bool
        blockNonEmpty [Label _] = False
        blockNonEmpty []        = False
        blockNonEmpty _         = True

        blockHasOnlyOneJump :: [Stmt] -> Bool
        blockHasOnlyOneJump = blockHasOnlyOneJump'
          where blockHasOnlyOneJump' (s:ss@(_:_)) =  not (isJump s)
                                                  && blockHasOnlyOneJump' ss
                blockHasOnlyOneJump' _ = True

                isJump = \case
                    Jump{}  -> True
                    CJump{} -> True
                    Ret     -> True
                    _       -> False

        checkCJumpFallback :: ControlFlowGraph -> Bool
        checkCJumpFallback ControlFlowGraph{..} = checkCJumpFallback' stmts
          where stmts = concat $ Graph.dfsWith' graphToList cfgGraph
                graphToList (_, _, Block{..}, _) = blockStmts

                checkCJumpFallback' (CJump _ _ _ _ fLab : s : ss) = case s of
                    Label l -> fLab == l && checkCJumpFallback' ss
                    _       -> False
                checkCJumpFallback' (_ : ss) = checkCJumpFallback' ss
                checkCJumpFallback' _        = True


showFuncName :: Frame f => IRFunction b f -> String
showFuncName = LazyText.unpack
             . Builder.toLazyText
             . Temp.labelBuilder
             . Frame.frameName
             . irFuncFrame
