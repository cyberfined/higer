module IRCanonTests (tests) where

import           Control.Monad          (forM_)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

import           BackendTestCases       (Input (..), TestCase (..), testCases)
import           Common                 (genericCanonicalizeIR, runInterpreter)
import           Data.List              (find)
import           Tiger.Frame            (Frame (..))
import           Tiger.IR               (Block (..), ControlFlowGraph (..), Expr (..),
                                         IRData (..), IRFunction (..), Stmt (..))
import           Tiger.RegMachine       (FrameEmulator, FrameRegister (..),
                                         InterpreterResult (..), ReturnRegister (..))
import           Tiger.Temp             (Temp)
import           Tiger.TextUtils        (toTextBuilder)

import qualified Data.Graph.Inductive   as Graph
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LazyText
import qualified Data.Text.Lazy.Builder as Builder

import qualified Tiger.Amd64            as Amd64
import qualified Tiger.Frame            as Frame
import qualified Tiger.Temp             as Temp

tests :: TestTree
tests = testGroup "IR canonicalization tests"
    [ amd64Tests
    ]

amd64Tests :: TestTree
amd64Tests = testGroup "AMD64 tests"
    [ runTestCases "semantic tests" (Proxy @Amd64.LinuxFrame) (Proxy @Amd64.IREmulator)
    , runLinearizationTestCases "linearization tests" (Proxy @Amd64.LinuxFrame)
    ]

runTestCases :: forall f e. FrameEmulator f e Temp Stmt
             => TestName
             -> Proxy f
             -> Proxy e
             -> TestTree
runTestCases name framePrx emuPrx = testGroup name $ map runTestCase testCases
  where runTestCase :: TestCase -> TestTree
        runTestCase TestCase{..} = testCase testName $ do
            irData <- compileToIR testSrc framePrx
            forM_ testInputs $ \Input{..} -> do
                let rv = ReturnRegister Temp.RV
                let fp = FrameRegister Temp.FP
                res <- runInterpreter rv fp emuPrx irData inputStdin
                let errText = "when interpret \n" ++ Text.unpack testSrc ++ "\n\n"
                            ++ "input: "
                            ++ Text.unpack inputStdin
                assertEqual errText
                            (inputStdout, inputExitCode)
                            (resOutput res, resCode res)

compileToIR :: forall f. Frame f
            => Text
            -> Proxy f
            -> IO (IRData (ControlFlowGraph Stmt) f)
compileToIR src prxy = genericCanonicalizeIR src prxy pure

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

        isLinearizedFunc :: IRFunction (ControlFlowGraph Stmt) f -> Bool
        isLinearizedFunc IRFunction{..} =
            all (blockIsCanon . fstBlockWithLabel funName . snd) blocks &&
            checkCJumpFallback irFuncBody
          where funName = Frame.frameName irFuncFrame
                blocks = Graph.labNodes $ cfgGraph irFuncBody

        fstBlockWithLabel :: Temp.Label -> Block Stmt -> Block Stmt
        fstBlockWithLabel funName b@Block{..}
          | blockLabel == funName = b { blockStmts = Label funName : blockStmts }
          | otherwise             = b

        blockIsCanon :: Block Stmt -> Bool
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

        checkCJumpFallback :: ControlFlowGraph Stmt -> Bool
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
             . toTextBuilder
             . Frame.frameName
             . irFuncFrame
