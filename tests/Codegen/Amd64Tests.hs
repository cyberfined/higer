{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Codegen.Amd64Tests
    ( genStmtTests
    , semanticTestCases
    , testRvFpAbsent
    ) where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Proxy             (Proxy (..))
import           Test.Tasty
import           Test.Tasty.HUnit

import           BackendTestCases       (TestCase (..))
import           Common                 (genericCodegen)
import           Tiger.Amd64            (Base (..), CallingConvention, Condition (..),
                                         Imul (..), Index (..), Instr (..), Offset (..),
                                         Operand (..), Reg (..), Scale (..), genStmt)
import           Tiger.Codegen          (TempReg (..))
import           Tiger.IR               (Block (..), ControlFlowGraph (..), IRData (..),
                                         IRFunction (..))
import           Tiger.Semant           (Type (..))
import           Tiger.Temp             (InitLabel (..), InitTemp (..), runTempM)
import           Tiger.TextUtils        (TextBuildable (..), intercalate)

import qualified Data.Graph.Inductive   as Graph
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Text.Lazy         as LazyText
import qualified Data.Text.Lazy.Builder as Builder

import qualified BackendTestCases       as Backend
import qualified Tiger.Amd64            as Amd64
import qualified Tiger.DList            as DList
import qualified Tiger.Frame            as Frame
import qualified Tiger.IR               as IR
import qualified Tiger.Temp             as Temp

newtype ShowInstr r = ShowInstr { getInstrs :: [Instr r] } deriving newtype Eq

instance TextBuildable r => Show (ShowInstr r) where
    show = LazyText.unpack
         . Builder.toLazyText
         . ("\n"<>)
         . intercalate "\n"
         . map toTextBuilder
         . getInstrs

newtype ShowReg r = ShowReg { getReg :: r } deriving newtype Eq

instance TextBuildable r => Show (ShowReg r) where
    show = LazyText.unpack . Builder.toLazyText . toTextBuilder . getReg

genStmtTests :: TestTree
genStmtTests = testGroup "genStmt tests" $ runTests (Proxy @Amd64.LinuxFrame)
    genStmtTestCases

genStmtTestCases :: (CallingConvention f, Frame.Reg f ~ Reg) => [Proxy f -> TestTree]
genStmtTestCases =
    [ test "label gen" (IR.Label (Temp.LabelText "l1")) [Label (Temp.LabelText "l1")]
    , test "jump gen" (IR.Jump (Temp.LabelText "dick")) [Jmp (Temp.LabelText "dick")]
    , \prxy -> test "ret gen" IR.Ret [Ret (Amd64.calleeSaveRegisters prxy)] prxy
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "negate" (IR.Move (IR.Temp t2) (IR.Binop IR.Sub (IR.Const 0) (IR.Temp t1)))
         [ Mov (Register $ Temp t2) (Register $ Temp t1)
         , Neg (Register $ Temp t2)
         ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "sub: dst and second operand are different"
          (IR.Move (IR.Temp t2) (IR.Binop IR.Sub (IR.Temp t2) (IR.Temp t1)))
          [ Mov (Register $ Temp t2) (Register $ Temp t2)
          , Sub (Register $ Temp t2) (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "sub: dst and second operand are the same"
          (IR.Move (IR.Temp t2) (IR.Binop IR.Sub (IR.Temp t1) (IR.Temp t2)))
          [ Mov (Register t100) (Register $ Temp t1)
          , Sub (Register t100) (Register $ Temp t2)
          , Mov (Register $ Temp t2) (Register t100)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "mul: 32-bit const #1" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Mul (IR.Const 12345) (IR.Temp t2)))
          [ Imul $ Imul3 (Register $ Temp t1) (Register $ Temp t2) (Const 12345)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "mul: 32-bit const #2" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Mul (IR.Temp t2) (IR.Const 12345)))
          [ Imul $ Imul3 (Register $ Temp t1) (Register $ Temp t2) (Const 12345)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "mul: 64-bit const #1" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Mul (IR.Const 68719476721) (IR.Temp t2)))
          [ Mov (Register $ Temp t1) (Const 68719476721)
          , Imul $ Imul2 (Register $ Temp t1) (Register $ Temp t2)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "mul: 64-bit const #2" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Mul (IR.Temp t2) (IR.Const 68719476721)))
          [ Mov (Register t100) (Const 68719476721)
          , Mov (Register $ Temp t1) (Register $ Temp t2)
          , Imul $ Imul2 (Register $ Temp t1) (Register t100)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "mul: dst and second operand are different"
          (IR.Move (IR.Temp t2) (IR.Binop IR.Mul (IR.Temp t1)
            (IR.Binop IR.Add (IR.Temp t1) (IR.Temp t2))))
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1)
                (Index $ Temp t2) (Scale 1))
          , Mov (Register $ Temp t2) (Register $ Temp t1)
          , Imul (Imul2 (Register $ Temp t2) (Register t100))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "mul: dst and second operand are the same"
          (IR.Move (IR.Temp t2) (IR.Binop IR.Mul
            (IR.Binop IR.Add (IR.Temp t1) (IR.Temp t2)) (IR.Temp t2)))
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1)
                (Index $ Temp t2) (Scale 1))
          , Imul (Imul2 (Register t100) (Register $ Temp t2))
          , Mov (Register $ Temp t2) (Register t100)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "div: the divider is a power of two" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Div (IR.Temp t2) (IR.Const 256)))
          [ Mov (Register $ Temp t1) (Register $ Temp t2)
          , Sar (Register $ Temp t1) (Const 8)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "div: the divider is a constant" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Div (IR.Temp t2) (IR.Const 257)))
          [ Mov (Register t100) (Const 257)
          , Mov (Register $ Reg Rax) (Register $ Temp t2)
          , Cqo
          , Idiv (Register t100)
          , Mov (Register $ Temp t1) (Register $ Reg Rax)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "div: the divider is an expression" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Div (IR.Temp t2) (IR.Binop IR.Add (IR.Temp t1) (IR.Const 256))))
          [ Lea (Register t100) (AddrRegBase (Offset 256) (Base $ Temp t1))
          , Mov (Register $ Reg Rax) (Register $ Temp t2)
          , Cqo
          , Idiv (Register t100)
          , Mov (Register $ Temp t1) (Register $ Reg Rax)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "div: the divident is a constant" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Div (IR.Const 123) (IR.Binop IR.Sub (IR.Temp t2) (IR.Temp t1))))
          [ Mov (Register t100) (Register $ Temp t2)
          , Sub (Register t100) (Register $ Temp t1)
          , Mov (Register $ Reg Rax) (Const 123)
          , Cqo
          , Idiv (Register t100)
          , Mov (Register $ Temp t1) (Register $ Reg Rax)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t3 = Temp.Temp 3
      in test "div: the divident is an expression" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Div (IR.Binop IR.Add (IR.Temp t3) (IR.Temp t2)) (IR.Temp t1)))
          [ Lea (Register $ Reg Rax) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t3)
                (Index $ Temp t2) (Scale 1))
          , Cqo
          , Idiv (Register $ Temp t1)
          , Mov (Register $ Temp t1) (Register $ Reg Rax)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t3 = Temp.Temp 3
          t4 = Temp.Temp 4
          t100 = Temp $ Temp.Temp 100
          t101 = Temp $ Temp.Temp 101
      in test "div: many divisions in one expression" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Div
            (IR.Binop IR.Div
                (IR.Binop IR.Add (IR.Binop IR.Div (IR.Temp t3) (IR.Temp t4)) (IR.Const 111))
                (IR.Binop IR.Sub (IR.Temp t2) (IR.Temp t1)))
            (IR.Const 257)))
          [ Mov (Register t100) (Const 257)
          , Mov (Register t101) (Register $ Temp t2)
          , Sub (Register t101) (Register $ Temp t1)
          , Mov (Register $ Reg Rax) (Register $ Temp t3)
          , Cqo
          , Idiv (Register $ Temp t4)
          , Mov (Register $ Reg Rax) (Register $ Reg Rax)
          , Lea (Register $ Reg Rax) (AddrRegBase (Offset 111) (Base $ Reg Rax))
          , Cqo
          , Idiv (Register t101)
          , Mov (Register $ Reg Rax) (Register $ Reg Rax)
          , Cqo
          , Idiv (Register t100)
          , Mov (Register $ Temp t1) (Register $ Reg Rax)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "move mem to mem" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Temp t1)
            (IR.Binop IR.Mul (IR.Temp t2) (IR.Const 8))))
          (IR.Mem (IR.Binop IR.Add (IR.Temp t1) (IR.Temp t2))))
          [ Mov (Register t100) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1)
                (Index $ Temp t2) (Scale 1))
          , Mov (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1) (Index $ Temp t2)
                (Scale 8)) (Register t100)
          ]
    , \prxy -> testGroup "effective address tests" $ runTests prxy effAddrTests
    , \prxy -> testGroup "cjump tests" $ runTests prxy cjumpTests
    ]

effAddrTests :: (CallingConvention f, Frame.Reg f ~ Reg) => [Proxy f -> TestTree]
effAddrTests =
    [ let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea offBaseIdxScale #1" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Const 5) (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1)))))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea offBaseIdxScale #2" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Const 5) (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 8)))))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea offBaseIdxScale #3" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Const 5) (IR.Binop IR.Add
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17)))))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea offBaseIdxScale #4" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))) (IR.Const 5)))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea offBaseIdxScale #5" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 8))) (IR.Const 5)))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea offBaseIdxScale #6" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))) (IR.Const 5)))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea offBaseIdxScale #7" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 8))
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))) (IR.Const 5)))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "lea offBaseIdxScale #8" (IR.Move (IR.Temp t2)
          (IR.Binop IR.Add (IR.Const 111) (IR.Binop IR.Add (IR.Temp t1)
          (IR.Binop IR.Div (IR.Temp t1) (IR.Const 32)))))
          [ Mov (Register t100) (Register $ Temp t1)
          , Sar (Register t100) (Const 5)
          , Lea (Register $ Temp t2) (AddrRegBaseIndex (Offset 111)
                (Just $ Base $ Temp t1) (Index t100) (Scale 1))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "lea offBaseIdxScale #9" (IR.Move (IR.Temp t2)
          (IR.Binop IR.Add (IR.Binop IR.Add (IR.Temp t1)
          (IR.Binop IR.Div (IR.Temp t1) (IR.Const 32))) (IR.Const 111)))
          [ Mov (Register t100) (Register $ Temp t1)
          , Sar (Register t100) (Const 5)
          , Lea (Register $ Temp t2) (AddrRegBaseIndex (Offset 111)
                (Just $ Base $ Temp t1) (Index t100) (Scale 1))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem offBaseIdxScale #1" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Const 5) (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))))))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem offBaseIdxScale #2" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Const 5) (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 8))))))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem offBaseIdxScale #3" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Const 5) (IR.Binop IR.Add
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))))))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem offBaseIdxScale #4" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))) (IR.Const 5))))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem offBaseIdxScale #5" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 8))) (IR.Const 5))))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem offBaseIdxScale #6" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))) (IR.Const 5))))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem offBaseIdxScale #7" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 8))
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))) (IR.Const 5))))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 5) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "load mem offBaseIdxScale #8" (IR.Move (IR.Temp t2)
          (IR.Mem (IR.Binop IR.Add (IR.Const 111) (IR.Binop IR.Add (IR.Temp t1)
          (IR.Binop IR.Div (IR.Temp t1) (IR.Const 32))))))
          [ Mov (Register t100) (Register $ Temp t1)
          , Sar (Register t100) (Const 5)
          , Mov (Register $ Temp t2) (AddrRegBaseIndex (Offset 111)
                (Just $ Base $ Temp t1) (Index t100) (Scale 1))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "load mem offBaseIdxScale #9" (IR.Move (IR.Temp t2)
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Add (IR.Temp t1)
          (IR.Binop IR.Div (IR.Temp t1) (IR.Const 32))) (IR.Const 111))))
          [ Mov (Register t100) (Register $ Temp t1)
          , Sar (Register t100) (Const 5)
          , Mov (Register $ Temp t2) (AddrRegBaseIndex (Offset 111)
                (Just $ Base $ Temp t1) (Index t100) (Scale 1))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBaseIdxScale #1" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Const 5) (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))))) (IR.Temp t1))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (AddrRegBaseIndex (Offset 5) (Just $ Base t100) (Index $ Temp t1) (Scale 8))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBaseIdxScale #2" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Const 5) (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 8))))) (IR.Temp t1))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (AddrRegBaseIndex (Offset 5) (Just $ Base t100) (Index $ Temp t1) (Scale 8))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBaseIdxScale #3" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Const 5) (IR.Binop IR.Add
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))))) (IR.Temp t1))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (AddrRegBaseIndex (Offset 5) (Just $ Base t100) (Index $ Temp t1) (Scale 8))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBaseIdxScale #4" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))) (IR.Const 5))) (IR.Temp t1))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (AddrRegBaseIndex (Offset 5) (Just $ Base t100) (Index $ Temp t1) (Scale 8))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBaseIdxScale #5" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))
              (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 8))) (IR.Const 5))) (IR.Temp t1))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (AddrRegBaseIndex (Offset 5) (Just $ Base t100) (Index $ Temp t1) (Scale 8))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBaseIdxScale #6" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))) (IR.Const 5))) (IR.Temp t1))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (AddrRegBaseIndex (Offset 5) (Just $ Base t100) (Index $ Temp t1) (Scale 8))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBaseIdxScale #7" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Add
              (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 8))
              (IR.Binop IR.Add (IR.Const 11) (IR.Const 17))) (IR.Const 5)))
              (IR.Temp t1))
          [ Mov (Register t100) (Const 17)
          , Lea (Register t100) (AddrRegBase (Offset 11) (Base t100))
          , Mov (AddrRegBaseIndex (Offset 5) (Just $ Base t100) (Index $ Temp t1) (Scale 8))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBaseIdxScale #8" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Const 111) (IR.Binop IR.Add (IR.Temp t1)
          (IR.Binop IR.Div (IR.Temp t1) (IR.Const 32))))) (IR.Temp t2))
          [ Mov (Register t100) (Register $ Temp t1)
          , Sar (Register t100) (Const 5)
          , Mov (AddrRegBaseIndex (Offset 111) (Just $ Base $ Temp t1)
                (Index t100) (Scale 1)) (Register $ Temp t2)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBaseIdxScale #9" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Add (IR.Temp t1)
          (IR.Binop IR.Div (IR.Temp t1) (IR.Const 32))) (IR.Const 111))) (IR.Temp t2))
          [ Mov (Register t100) (Register $ Temp t1)
          , Sar (Register t100) (Const 5)
          , Mov (AddrRegBaseIndex (Offset 111) (Just $ Base $ Temp t1)
                (Index t100) (Scale 1)) (Register $ Temp t2)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "lea offIdxScale #1" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Const 666) (IR.Binop IR.Mul (IR.Const 4)
          (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78)))))
          [ Lea (Register $ Temp t1) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 666) Nothing
                (Index $ Temp t1) (Scale 4))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "lea offIdxScale #2" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Const 666) (IR.Binop IR.Mul
          (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78)) (IR.Const 8))))
          [ Lea (Register $ Temp t1) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 666) Nothing
                (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "lea offIdxScale #3" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Binop IR.Mul
          (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78)) (IR.Const 2)) (IR.Const 666)))
          [ Lea (Register $ Temp t1) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 666) Nothing
                (Index $ Temp t1) (Scale 2))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "lea offIdxScale #4" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Binop IR.Mul
          (IR.Const 8) (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78))) (IR.Const 666)))
          [ Lea (Register $ Temp t1) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 666) Nothing
                (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "load mem offIdxScale #1" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Const 666) (IR.Binop IR.Mul (IR.Const 4)
          (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78))))))
          [ Lea (Register $ Temp t1) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 666) Nothing
                (Index $ Temp t1) (Scale 4))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "load mem offIdxScale #2" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Const 666) (IR.Binop IR.Mul
          (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78)) (IR.Const 8)))))
          [ Lea (Register $ Temp t1) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 666) Nothing
                (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "load mem offIdxScale #3" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Mul
          (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78)) (IR.Const 2)) (IR.Const 666))))
          [ Lea (Register $ Temp t1) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 666) Nothing
                (Index $ Temp t1) (Scale 2))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "load mem offIdxScale #4" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Mul
          (IR.Const 8) (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78))) (IR.Const 666))))
          [ Lea (Register $ Temp t1) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 666) Nothing
                (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "store mem offIdxScale #1" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Const 666) (IR.Binop IR.Mul (IR.Const 4)
          (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78))))) (IR.Temp t1))
          [ Lea (Register t100) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Mov (AddrRegBaseIndex (Offset 666) Nothing (Index t100) (Scale 4))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "store mem offIdxScale #2" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Const 666) (IR.Binop IR.Mul
          (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78)) (IR.Const 8)))) (IR.Temp t1))
          [ Lea (Register t100) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Mov (AddrRegBaseIndex (Offset 666) Nothing (Index t100) (Scale 8))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "store mem offIdxScale #3" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Mul
          (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78)) (IR.Const 2)) (IR.Const 666)))
          (IR.Temp t1))
          [ Lea (Register t100) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Mov (AddrRegBaseIndex (Offset 666) Nothing (Index t100) (Scale 2))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "store mem offIdxScale #4" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Mul
          (IR.Const 8) (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 78))) (IR.Const 666)))
          (IR.Temp t1))
          [ Lea (Register t100) (AddrRegBase (Offset $ -78) (Base $ Temp t2))
          , Mov (AddrRegBaseIndex (Offset 666) Nothing (Index t100) (Scale 8))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea baseScaleIndex #1" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add
            (IR.Binop IR.Mul (IR.Const 4) (IR.Temp t1))
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1))))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 0) (Just $ Base t100)
              (Index $ Temp t1) (Scale 4))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea baseScaleIndex #2" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add
            (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 2))
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1))))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 0) (Just $ Base t100)
              (Index $ Temp t1) (Scale 2))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea baseScaleIndex #3" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1))
            (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1))))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 0) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea baseScaleIndex #4" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1))
            (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 4))))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 0) (Just $ Base t100)
              (Index $ Temp t1) (Scale 4))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem baseScaleIndex #1" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add
            (IR.Binop IR.Mul (IR.Const 4) (IR.Temp t1))
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1)))))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 0) (Just $ Base t100)
              (Index $ Temp t1) (Scale 4))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem baseScaleIndex #2" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add
            (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 2))
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1)))))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 0) (Just $ Base t100)
              (Index $ Temp t1) (Scale 2))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem baseScaleIndex #3" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1))
            (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1)))))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 0) (Just $ Base t100)
              (Index $ Temp t1) (Scale 8))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem baseScaleIndex #4" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1))
            (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 4)))))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 0) (Just $ Base t100)
              (Index $ Temp t1) (Scale 4))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem baseScaleIndex #1" (IR.Move
          (IR.Mem (IR.Binop IR.Add
            (IR.Binop IR.Mul (IR.Const 4) (IR.Temp t1))
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1)))) (IR.Temp t1))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Mov (AddrRegBaseIndex (Offset 0) (Just $ Base t100) (Index $ Temp t1) (Scale 4))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem baseScaleIndex #2" (IR.Move
          (IR.Mem (IR.Binop IR.Add
            (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 2))
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1)))) (IR.Temp t1))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Mov (AddrRegBaseIndex (Offset 0) (Just $ Base t100) (Index $ Temp t1) (Scale 2))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem baseScaleIndex #3" (IR.Move
          (IR.Mem (IR.Binop IR.Add
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1))
            (IR.Binop IR.Mul (IR.Const 8) (IR.Temp t1)))) (IR.Temp t1))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Mov (AddrRegBaseIndex (Offset 0) (Just $ Base t100) (Index $ Temp t1) (Scale 8))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem baseScaleIndex #4" (IR.Move
          (IR.Mem (IR.Binop IR.Add
            (IR.Binop IR.Add (IR.Const 10) (IR.Temp t1))
            (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 4)))) (IR.Temp t1))
          [ Lea (Register t100) (AddrRegBase (Offset 10) (Base $ Temp t1))
          , Mov (AddrRegBaseIndex (Offset 0) (Just $ Base t100) (Index $ Temp t1) (Scale 4))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "lea offBase #1" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Const 777) (IR.Binop IR.Mul (IR.Temp t2) (IR.Const 11))))
          [ Imul (Imul3 (Register $ Temp t1) (Register $ Temp t2) (Const 11))
          , Lea (Register $ Temp t1) (AddrRegBase (Offset 777) (Base $ Temp t1))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "lea offBase #2" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Binop IR.Mul (IR.Temp t2) (IR.Const 11)) (IR.Const 777)))
          [ Imul (Imul3 (Register $ Temp t1) (Register $ Temp t2) (Const 11))
          , Lea (Register $ Temp t1) (AddrRegBase (Offset 777) (Base $ Temp t1))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "load mem offBase #1" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Const 777) (IR.Binop IR.Mul (IR.Temp t2)
                  (IR.Const 11)))))
          [ Imul (Imul3 (Register $ Temp t1) (Register $ Temp t2) (Const 11))
          , Mov (Register $ Temp t1) (AddrRegBase (Offset 777) (Base $ Temp t1))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "load mem offBase #2" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Mul (IR.Temp t2) (IR.Const 11))
                  (IR.Const 777))))
          [ Imul (Imul3 (Register $ Temp t1) (Register $ Temp t2) (Const 11))
          , Mov (Register $ Temp t1) (AddrRegBase (Offset 777) (Base $ Temp t1))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBase #1" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Const 777) (IR.Binop IR.Mul (IR.Temp t2)
                  (IR.Const 11)))) (IR.Temp t1))
          [ Imul (Imul3 (Register t100) (Register $ Temp t2) (Const 11))
          , Mov (AddrRegBase (Offset 777) (Base t100)) (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "store mem offBase #2" (IR.Move
          (IR.Mem (IR.Binop IR.Add (IR.Binop IR.Mul (IR.Temp t2) (IR.Const 11))
                  (IR.Const 777))) (IR.Temp t1))
          [ Imul (Imul3 (Register t100) (Register $ Temp t2) (Const 11))
          , Mov (AddrRegBase (Offset 777) (Base t100)) (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "lea add" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Add (IR.Binop IR.Add (IR.Const 5) (IR.Const 10)) (IR.Temp t1)))
          [ Mov (Register t100) (Const 10)
          , Lea (Register t100) (AddrRegBase (Offset 5) (Base t100))
          , Lea (Register $ Temp t1) (AddrRegBaseIndex (Offset 0) (Just $ Base t100)
                (Index $ Temp t1) (Scale 1))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem add" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Add
            (IR.Binop IR.Add (IR.Const 5) (IR.Const 10)) (IR.Temp t1))))
          [ Mov (Register t100) (Const 10)
          , Lea (Register t100) (AddrRegBase (Offset 5) (Base t100))
          , Mov (Register $ Temp t1) (AddrRegBaseIndex (Offset 0) (Just $ Base t100)
                (Index $ Temp t1) (Scale 1))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "store mem add" (IR.Move
          (IR.Mem (IR.Binop IR.Add
            (IR.Binop IR.Add (IR.Const 5) (IR.Const 10)) (IR.Temp t1))) (IR.Temp t1))
          [ Mov (Register t100) (Const 10)
          , Lea (Register t100) (AddrRegBase (Offset 5) (Base t100))
          , Mov (AddrRegBaseIndex (Offset 0) (Just $ Base t100) (Index $ Temp t1) (Scale 1))
                (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
      in test "lea sub const" (IR.Move (IR.Temp t1)
          (IR.Binop IR.Sub (IR.Temp t1) (IR.Const 541)))
          [ Lea (Register $ Temp t1) (AddrRegBase (Offset $ -541) (Base $ Temp t1))
          ]
    , let t1 = Temp.Temp 1
      in test "load mem sub const" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Sub (IR.Temp t1) (IR.Const 541))))
          [ Mov (Register $ Temp t1) (AddrRegBase (Offset $ -541) (Base $ Temp t1))
          ]
    , let t1 = Temp.Temp 1
      in test "store mem sub const" (IR.Move
          (IR.Mem (IR.Binop IR.Sub (IR.Temp t1) (IR.Const 541))) (IR.Temp t1))
          [ Mov (AddrRegBase (Offset $ -541) (Base $ Temp t1)) (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "lea mul const #1" (IR.Move (IR.Temp t2)
          (IR.Binop IR.Mul (IR.Binop IR.Add (IR.Temp t2) (IR.Temp t1)) (IR.Const 4)))
          [ Lea (Register $ Temp t2) (AddrRegBaseIndex (Offset 0)
                (Just $ Base $ Temp t2) (Index $ Temp t1) (Scale 1))
          , Lea (Register $ Temp t2) (AddrRegBaseIndex (Offset 0) Nothing
                (Index $ Temp t2) (Scale 4))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "lea mul const #2" (IR.Move (IR.Temp t2)
          (IR.Binop IR.Mul (IR.Const 4) (IR.Binop IR.Add (IR.Temp t2) (IR.Temp t1))))
          [ Lea (Register $ Temp t2) (AddrRegBaseIndex (Offset 0)
                (Just $ Base $ Temp t2) (Index $ Temp t1) (Scale 1))
          , Lea (Register $ Temp t2) (AddrRegBaseIndex (Offset 0) Nothing
                (Index $ Temp t2) (Scale 4))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "load mem mul const #1" (IR.Move (IR.Temp t2)
          (IR.Mem (IR.Binop IR.Mul (IR.Binop IR.Add (IR.Temp t2) (IR.Temp t1))
            (IR.Const 4))))
          [ Lea (Register $ Temp t2) (AddrRegBaseIndex (Offset 0)
                (Just $ Base $ Temp t2) (Index $ Temp t1) (Scale 1))
          , Mov (Register $ Temp t2) (AddrRegBaseIndex (Offset 0) Nothing
                (Index $ Temp t2) (Scale 4))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "load mem mul const #2" (IR.Move (IR.Temp t2)
          (IR.Mem (IR.Binop IR.Mul (IR.Const 4) (IR.Binop IR.Add (IR.Temp t2)
            (IR.Temp t1)))))
          [ Lea (Register $ Temp t2) (AddrRegBaseIndex (Offset 0)
                (Just $ Base $ Temp t2) (Index $ Temp t1) (Scale 1))
          , Mov (Register $ Temp t2) (AddrRegBaseIndex (Offset 0) Nothing
                (Index $ Temp t2) (Scale 4))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "store mem mul const #1" (IR.Move
          (IR.Mem (IR.Binop IR.Mul (IR.Binop IR.Add (IR.Temp t2) (IR.Temp t1))
            (IR.Const 4))) (IR.Temp t2))
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0)
                (Just $ Base $ Temp t2) (Index $ Temp t1) (Scale 1))
          , Mov (AddrRegBaseIndex (Offset 0) Nothing (Index t100) (Scale 4))
                (Register $ Temp t2)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "store mem mul const #2" (IR.Move
          (IR.Mem (IR.Binop IR.Mul (IR.Const 4) (IR.Binop IR.Add (IR.Temp t2)
            (IR.Temp t1)))) (IR.Temp t2))
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0)
                (Just $ Base $ Temp t2) (Index $ Temp t1) (Scale 1))
          , Mov (AddrRegBaseIndex (Offset 0) Nothing (Index t100) (Scale 4))
                (Register $ Temp t2)
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "load mem temp" (IR.Move (IR.Temp t1) (IR.Mem (IR.Temp t2)))
          [ Mov (Register $ Temp t1) (AddrRegBase (Offset 0) (Base $ Temp t2))
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "store mem temp" (IR.Move (IR.Mem (IR.Temp t2)) (IR.Temp t1))
          [ Mov (AddrRegBase (Offset 0) (Base $ Temp t2)) (Register $ Temp t1)
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "load mem expr" (IR.Move (IR.Temp t1)
          (IR.Mem (IR.Binop IR.Div
            (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 7))
            (IR.Const 3))))
          [ Mov (Register t100) (Const 3)
          , Imul $ Imul3 (Register $ Reg Rax) (Register $ Temp t1) (Const 7)
          , Cqo
          , Idiv (Register t100)
          , Mov (Register $ Temp t1) (Register $ Reg Rax)
          , Mov (Register $ Temp t1) (AddrRegBase (Offset 0) (Base $ Temp t1))
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
          t101 = Temp $ Temp.Temp 101
      in test "store mem expr" (IR.Move
          (IR.Mem (IR.Binop IR.Div
            (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 7))
            (IR.Const 3))) (IR.Temp t1))
          [ Mov (Register t101) (Const 3)
          , Imul $ Imul3 (Register $ Reg Rax) (Register $ Temp t1) (Const 7)
          , Cqo
          , Idiv (Register t101)
          , Mov (Register t100) (Register $ Reg Rax)
          , Mov (AddrRegBase (Offset 0) (Base t100)) (Register $ Temp t1)
          ]
    ]

cjumpTests :: (CallingConvention f, Frame.Reg f ~ Reg) => [Proxy f -> TestTree]
cjumpTests =
    [ let t1 = Temp.Temp 1
      in test "cmp mem temp 32-bit const #1" (IR.CJump IR.Gt
          (IR.Mem (IR.Temp t1)) (IR.Const 72) tLab fLab)
          [ Cmp (AddrRegBase (Offset 0) (Base $ Temp t1)) (Const 72)
          , Jcc Gt tLab fLab
          ]
    , let t1 = Temp.Temp 1
      in test "cmp mem temp 32-bit const #2" (IR.CJump IR.Gt
          (IR.Const 72) (IR.Mem (IR.Temp t1)) tLab fLab)
          [ Cmp (AddrRegBase (Offset 0) (Base $ Temp t1)) (Const 72)
          , Jcc Gt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "cmp mem temp 64-bit const #1" (IR.CJump IR.Gt
          (IR.Mem (IR.Temp t1)) (IR.Const 1095504202102) tLab fLab)
          [ Mov (Register t100) (Const 1095504202102)
          , Cmp (AddrRegBase (Offset 0) (Base $ Temp t1)) (Register t100)
          , Jcc Gt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t100 = Temp $ Temp.Temp 100
      in test "cmp mem temp 64-bit const #2" (IR.CJump IR.Gt
          (IR.Const 1095504202102) (IR.Mem (IR.Temp t1)) tLab fLab)
          [ Mov (Register t100) (Const 1095504202102)
          , Cmp (AddrRegBase (Offset 0) (Base $ Temp t1)) (Register t100)
          , Jcc Gt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "cmp mem temp #1" (IR.CJump IR.Lt
          (IR.Temp t2) (IR.Mem (IR.Binop IR.Add (IR.Temp t1) (IR.Const 64))) tLab fLab)
          [ Cmp (AddrRegBase (Offset 64) (Base $ Temp t1)) (Register $ Temp t2)
          , Jcc Lt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
      in test "cmp mem temp #2" (IR.CJump IR.Le
          (IR.Mem (IR.Binop IR.Add (IR.Temp t1) (IR.Const 64))) (IR.Temp t2) tLab fLab)
          [ Cmp (AddrRegBase (Offset 64) (Base $ Temp t1)) (Register $ Temp t2)
          , Jcc Le tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp mem expr #1" (IR.CJump IR.Eq
          (IR.Mem (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 4)))
                  (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 32)) tLab fLab)
          [ Lea (Register t100) (AddrRegBase (Offset $ -32) (Base $ Temp t2))
          , Cmp (AddrRegBaseIndex (Offset 0) Nothing (Index $ Temp t1) (Scale 4))
                (Register t100)
          , Jcc Eq tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp mem expr #2" (IR.CJump IR.Ne
                  (IR.Binop IR.Sub (IR.Temp t2) (IR.Const 32))
                  (IR.Mem (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 4))) tLab fLab)
          [ Lea (Register t100) (AddrRegBase (Offset $ -32) (Base $ Temp t2))
          , Cmp (AddrRegBaseIndex (Offset 0) Nothing (Index $ Temp t1) (Scale 4))
                (Register t100)
          , Jcc Ne tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
          t101 = Temp $ Temp.Temp 101
      in test "cmp mem expr #3" (IR.CJump IR.Lt
                  (IR.Mem (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 12)))
                  (IR.Binop IR.Sub (IR.Const 0) (IR.Temp t2)) tLab fLab)
          [ Imul $ Imul3 (Register t100) (Register $ Temp t1) (Const 12)
          , Mov (Register t101) (Register $ Temp t2)
          , Neg (Register t101)
          , Cmp (AddrRegBase (Offset 0) (Base t100)) (Register t101)
          , Jcc Lt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
          t101 = Temp $ Temp.Temp 101
      in test "cmp mem expr #4" (IR.CJump IR.Lt
                  (IR.Binop IR.Sub (IR.Const 0) (IR.Temp t2))
                  (IR.Mem (IR.Binop IR.Mul (IR.Temp t1) (IR.Const 12))) tLab fLab)
          [ Imul $ Imul3 (Register t100) (Register $ Temp t1) (Const 12)
          , Mov (Register t101) (Register $ Temp t2)
          , Neg (Register t101)
          , Cmp (AddrRegBase (Offset 0) (Base t100)) (Register t101)
          , Jcc Lt tLab fLab
          ]
    , let t1 = Temp.Temp 1
      in test "cmp temp zero #1" (IR.CJump IR.Eq (IR.Const 0) (IR.Temp t1) tLab fLab)
          [ Test (Register $ Temp t1) (Register $ Temp t1)
          , Jcc Eq tLab fLab
          ]
    , let t1 = Temp.Temp 1
      in test "cmp temp zero #2" (IR.CJump IR.Ne (IR.Temp t1) (IR.Const 0) tLab fLab)
          [ Test (Register $ Temp t1) (Register $ Temp t1)
          , Jcc Ne tLab fLab
          ]
    , let t1 = Temp.Temp 1
      in test "cmp temp zero #3" (IR.CJump IR.Ge (IR.Const 0) (IR.Temp t1) tLab fLab)
          [ Test (Register $ Temp t1) (Register $ Temp t1)
          , Jcc Ge tLab fLab
          ]
    , let t1 = Temp.Temp 1
      in test "cmp temp zero #4" (IR.CJump IR.Lt (IR.Temp t1) (IR.Const 0) tLab fLab)
          [ Test (Register $ Temp t1) (Register $ Temp t1)
          , Jcc Lt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp expr zero #1" (IR.CJump IR.Eq
          (IR.Binop IR.Add (IR.Temp t1) (IR.Temp t2))
          (IR.Const 0) tLab fLab)
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1)
                (Index $ Temp t2) (Scale 1))
          , Test (Register t100) (Register t100)
          , Jcc Eq tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp expr zero #2" (IR.CJump IR.Ne
          (IR.Const 0)
          (IR.Binop IR.Add (IR.Temp t1) (IR.Temp t2)) tLab fLab)
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1)
                (Index $ Temp t2) (Scale 1))
          , Test (Register t100) (Register t100)
          , Jcc Ne tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp expr zero #3" (IR.CJump IR.Ne
          (IR.Binop IR.Add (IR.Temp t1) (IR.Temp t2))
          (IR.Const 0) tLab fLab)
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1)
                (Index $ Temp t2) (Scale 1))
          , Test (Register t100) (Register t100)
          , Jcc Ne tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp expr zero #4" (IR.CJump IR.Gt
          (IR.Const 0)
          (IR.Binop IR.Add (IR.Temp t1) (IR.Temp t2)) tLab fLab)
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1)
                (Index $ Temp t2) (Scale 1))
          , Test (Register t100) (Register t100)
          , Jcc Gt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp expr zero #5" (IR.CJump IR.Gt
          (IR.Binop IR.Add (IR.Temp t1) (IR.Temp t2))
          (IR.Const 0) tLab fLab)
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1)
                (Index $ Temp t2) (Scale 1))
          , Test (Register t100) (Register t100)
          , Jcc Gt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp expr zero #6" (IR.CJump IR.Le
          (IR.Const 0)
          (IR.Binop IR.Add (IR.Temp t1) (IR.Temp t2)) tLab fLab)
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1)
                (Index $ Temp t2) (Scale 1))
          , Test (Register t100) (Register t100)
          , Jcc Le tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp expr zero #7" (IR.CJump IR.Ne
          (IR.Binop IR.Add (IR.Temp t1) (IR.Temp t2))
          (IR.Const 0) tLab fLab)
          [ Lea (Register t100) (AddrRegBaseIndex (Offset 0) (Just $ Base $ Temp t1)
                (Index $ Temp t2) (Scale 1))
          , Test (Register t100) (Register t100)
          , Jcc Ne tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp expr 32-bit const #1" (IR.CJump IR.Lt
          (IR.Binop IR.Mul (IR.Temp t1) (IR.Temp t2))
          (IR.Const 12378) tLab fLab)
          [ Mov (Register t100) (Register $ Temp t1)
          , Imul $ Imul2 (Register t100) (Register $ Temp t2)
          , Cmp (Register t100) (Const 12378)
          , Jcc Lt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
      in test "cmp expr 32-bit const #2" (IR.CJump IR.Ge
          (IR.Const 12378)
          (IR.Binop IR.Div (IR.Temp t1) (IR.Temp t2)) tLab fLab)
          [ Mov (Register $ Reg Rax) (Register $ Temp t1)
          , Cqo
          , Idiv (Register $ Temp t2)
          , Mov (Register t100) (Register $ Reg Rax)
          , Cmp (Register t100) (Const 12378)
          , Jcc Lt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
          t101 = Temp $ Temp.Temp 101
      in test "cmp expr 64-bit const #1" (IR.CJump IR.Lt
          (IR.Binop IR.Mul (IR.Temp t1) (IR.Temp t2))
          (IR.Const 734434937925) tLab fLab)
          [ Mov (Register t100) (Register $ Temp t1)
          , Imul $ Imul2 (Register t100) (Register $ Temp t2)
          , Mov (Register t101) (Const 734434937925)
          , Cmp (Register t100) (Register t101)
          , Jcc Lt tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
          t101 = Temp $ Temp.Temp 101
      in test "cmp expr 64-bit const #2" (IR.CJump IR.Ge
          (IR.Const 734434937925)
          (IR.Binop IR.Div (IR.Temp t1) (IR.Temp t2)) tLab fLab)
          [ Mov (Register t100) (Const 734434937925)
          , Mov (Register $ Reg Rax) (Register $ Temp t1)
          , Cqo
          , Idiv (Register $ Temp t2)
          , Mov (Register t101) (Register $ Reg Rax)
          , Cmp (Register t100) (Register t101)
          , Jcc Ge tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
          t101 = Temp $ Temp.Temp 101
      in test "cmp expr with expr" (IR.CJump IR.Ne
          (IR.Binop IR.Sub (IR.Temp t1) (IR.Temp t2))
          (IR.Binop IR.Div (IR.Temp t2) (IR.Const 32)) tLab fLab)
          [ Mov (Register t100) (Register $ Temp t1)
          , Sub (Register t100) (Register $ Temp t2)
          , Mov (Register t101) (Register $ Temp t2)
          , Sar (Register t101) (Const 5)
          , Cmp (Register t100) (Register t101)
          , Jcc Ne tLab fLab
          ]
    , let t1 = Temp.Temp 1
          t2 = Temp.Temp 2
          t100 = Temp $ Temp.Temp 100
          t101 = Temp $ Temp.Temp 101
      in test "cmp mem with mem" (IR.CJump IR.Gt
          (IR.Mem (IR.Binop IR.Sub (IR.Temp t1) (IR.Temp t2)))
          (IR.Mem (IR.Binop IR.Div (IR.Temp t2) (IR.Const 32))) tLab fLab)
          [ Mov (Register t100) (Register $ Temp t1)
          , Sub (Register t100) (Register $ Temp t2)
          , Mov (Register t101) (Register $ Temp t2)
          , Sar (Register t101) (Const 5)
          , Mov (Register t101) (AddrRegBase (Offset 0) (Base t101))
          , Cmp (AddrRegBase (Offset 0) (Base t100)) (Register t101)
          , Jcc Gt tLab fLab
          ]
    ]
  where tLab = Temp.LabelText "t"
        fLab = Temp.LabelText "f"

runTests :: CallingConvention f => Proxy f -> [Proxy f -> TestTree] -> [TestTree]
runTests prxy = map ($ prxy)

test :: forall f. (CallingConvention f, Frame.Reg f ~ Reg)
     => TestName
     -> IR.Stmt
     -> [Instr (TempReg Reg)]
     -> Proxy f
     -> TestTree
test name stmt expInstrs _ = testCase name $ runTempM initTemp initLabel $ do
    actInstrs <- ShowInstr . DList.toList <$> genStmt @f HashMap.empty TUnit stmt
    liftIO $ assertEqual "unexpected codegen result" (ShowInstr expInstrs) actInstrs
  where initTemp = InitTemp 100
        initLabel = InitLabel 100

semanticTestCases :: [TestCase]
semanticTestCases =
    [ Backend.test "many divisions in one expression"
        [ "let"
        , Backend.readLineSrc
        , Backend.itoaSrc
        , Backend.atoiSrc
        , "  var t1 := atoi(readLine())"
        , "  var t2 := atoi(readLine())"
        , "  var t3 := atoi(readLine())"
        , "  var t4 := atoi(readLine())"
        , "in"
        , "  t1 := (t3 / t4 + 1211) / (t2 - t1) / 100;"
        , "  print(itoa(t1))"
        , "end"
        ]
        [ ("30\n25\n127\n11", "-3", 0)
        , ("25\n30\n145\n13", "2", 0)
        ]
    , Backend.test "division by power of two"
        [ "let"
        , Backend.readLineSrc
        , Backend.itoaSrc
        , Backend.atoiSrc
        , "  var t1 := atoi(readLine())"
        , "  var t2 := atoi(readLine())"
        , "in"
        , "  t1 := (t1 + t2) / 128;"
        , "  print(itoa(t1))"
        , "end"
        ]
        [ ("250\n6", "2", 0)
        , ("-145\n-90", "-2", 0)
        ]
    , Backend.test "multiplication by 32-bit constant"
        [ "let"
        , Backend.readLineSrc
        , Backend.itoaSrc
        , Backend.atoiSrc
        , "  var t1 := atoi(readLine())"
        , "  var t2 := atoi(readLine())"
        , "in"
        , "  t1 := (t1 + t2) * 3456;"
        , "  print(itoa(t1))"
        , "end"
        ]
        [ ("250\n6", "884736", 0)
        , ("-145\n-90", "-812160", 0)
        ]
    , Backend.test "multiplication by 64-bit constant"
        [ "let"
        , Backend.readLineSrc
        , Backend.itoaSrc
        , Backend.atoiSrc
        , "  var t1 := atoi(readLine())"
        , "  var t2 := atoi(readLine())"
        , "in"
        , "  t1 := (t1 + t2) * 1095233407249;"
        , "  print(itoa(t1))"
        , "end"
        ]
        [ ("1\n2", "3285700221747", 0)
        , ("-10\n5", "-5476167036245", 0)
        ]
    , Backend.test "condition with 64-bit constant"
        [ "let"
        , Backend.readLineSrc
        , Backend.atoiSrc
        , "  var t1 := atoi(readLine())"
        , "in"
        , "  if t1 <= 734434937925 then print(\"LE\") else print(\"GT\")"
        , "end"
        ]
        [ ("1", "LE", 0)
        , ("734434937927", "GT", 0)
        ]
    ]

testRvFpAbsent :: forall f. ( CallingConvention f
                            , Frame.Instr f ~ Instr
                            , Frame.Reg f ~ Reg
                            )
               => Proxy f
               -> [TestCase]
               -> TestTree
testRvFpAbsent prxy = testGroup "test rv fp absent" .  map runTestCase
  where runTestCase TestCase{..} = testCase testName $ do
            IRData{..} <- genericCodegen testSrc prxy pure
            mapM_ assertFunction irFunctions

        assertFunction IRFunction{..} = do
            forM_ blocks $ \(_, Block{..}) -> mapM_ assertInstruction blockStmts
          where blocks = Graph.labNodes $ cfgGraph irFuncBody

        assertInstruction :: Instr (TempReg Reg) -> Assertion
        assertInstruction i = case i of
            Add dst src -> assertOperand i dst >> assertOperand i src
            Sub dst src -> assertOperand i dst >> assertOperand i src
            Xor dst src -> assertOperand i dst >> assertOperand i src
            Lea dst src -> assertOperand i dst >> assertOperand i src
            Sal dst src -> assertOperand i dst >> assertOperand i src
            Sar dst src -> assertOperand i dst >> assertOperand i src
            Imul imul -> case imul of
                Imul2 dst src -> assertOperand i dst >> assertOperand i src
                Imul3 dst op1 op2 -> assertOperand i dst
                                  >> assertOperand i op1
                                  >> assertOperand i op2
            Idiv src -> assertOperand i src
            Cqo -> pure ()
            Mov dst src -> assertOperand i dst >> assertOperand i src
            Test dst src -> assertOperand i dst >> assertOperand i src
            Cmp dst src -> assertOperand i dst >> assertOperand i src
            Jmp{} -> pure ()
            Jcc{} -> pure ()
            Ret{} -> pure ()
            Label{} -> pure ()
            Call{} -> pure ()
            Push src -> assertOperand i src
            Neg dst -> assertOperand i dst

        assertOperand :: Instr (TempReg Reg) -> Operand (TempReg Reg) t -> Assertion
        assertOperand i = \case
            AddrRegBase _ (Base (Temp r)) -> assertTemp i r
            AddrRegBaseIndex _ mBase (Index (Temp idx)) _ -> do
                case mBase of
                    Just (Base (Temp base)) -> assertTemp i base
                    _                       -> pure ()
                assertTemp i idx
            Register (Temp r) -> assertTemp i r
            _ -> pure ()

        assertTemp :: Instr (TempReg Reg) -> Temp.Temp -> Assertion
        assertTemp i t =
            let prefix = "instruction " ++ show (ShowInstr [i])
                rvMsg = prefix ++ "\ncontains RV temp"
                fpMsg = prefix ++ "\ncontains FP temp"
            in assertBool rvMsg (t /= Temp.RV) >> assertBool fpMsg (t /= Temp.FP)
