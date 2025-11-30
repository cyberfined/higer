module RegAlloc.Amd64Tests (tests) where

import           Data.List.NonEmpty   (NonEmpty (..))
import           Test.Tasty
import           Test.Tasty.HUnit

import           Common               (showBuildable)
import           Tiger.Amd64          (Base (..), CallArgs (..), CallDefs (..), Imul (..),
                                       Index (..), Instr (..), Offset (..), Operand (..),
                                       Reg (..), Scale (..))
import           Tiger.Codegen        (TempReg (..))
import           Tiger.IR             (Block (..), ControlFlowGraph (..), Neighs (..))
import           Tiger.RegAlloc       (Interval (..), RegMap (..), RegState (..),
                                       liveRanges)
import           Tiger.Temp           (Label (..))
import           Tiger.TextUtils      (TextBuildable (..))

import qualified Data.Graph.Inductive as Graph
import qualified Data.HashMap.Strict  as HashMap

import qualified Tiger.Amd64          as Amd64
import qualified Tiger.EnumMap        as EnumMap
import qualified Tiger.Temp           as Temp

newtype ShowRegMap r = ShowRegMap { getRegMap :: RegMap r } deriving newtype Eq

instance (TextBuildable r, Enum r) => Show (ShowRegMap r) where
  show = showBuildable . getRegMap

tests :: TestTree
tests = testGroup "AMD64 tests"
  [ liveRangesTests
  ]

liveRangesTests :: TestTree
liveRangesTests = testGroup "liveRanges tests"
  [ test "Test 1" testCFG1 expState1
  , test "Test 2" testCFG2 expState2
  , test "Test 3" testLoopCFG3 expState3
  , test "Test 4" testLoopCFG4 expState4
  ]
  where test :: TestName
             -> ControlFlowGraph (Instr (TempReg Reg))
             -> RegMap (TempReg Reg)
             -> TestTree
        test name cfg expState = testCase name $ do
          let actState = liveRanges cfg
          assertEqual "wrong live ranges" (ShowRegMap expState) (ShowRegMap actState)

        expState1 =
          let callerSaveRanges = occ 6 6 :| [occ 11 11, occ 18 18, occ 25 25]
          in RegMap $ EnumMap.fromList
             [ (Reg Rbx, occ 0 31 :| [])
             , (Reg R12, occ 0 31 :| [])
             , (Reg R13, occ 0 31 :| [])
             , (Reg R14, occ 0 31 :| [])
             , (Reg R15, occ 0 31 :| [])
             , (Reg Rax, occ 6 7 :| [occ 11 12, occ 18 19, occ 25 26, occ 30 31])
             , (Reg Rsi, occ 6 6 :| [occ 9 11, occ 16 18, occ 23 25])
             , (Reg Rdi, occ 6 6 :| [occ 10 11, occ 17 18, occ 24 25])
             , (Reg Rcx, callerSaveRanges)
             , (Reg Rdx, callerSaveRanges)
             , (Reg R8, callerSaveRanges)
             , (Reg R9, callerSaveRanges)
             , (Reg R10, callerSaveRanges)
             , (Reg R11, callerSaveRanges)
             , (Reg Rsp, occ 0 28 :| [])
             , (temp 0, occ 0 24 :| [occ 27 30])
             , (temp 1, occ 8 23 :| [])
             , (temp 33, occ 7 8 :| [])
             , (temp 34, occ 12 13 :| [])
             , (temp 35, occ 19 20 :| [])
             , (temp 36, occ 26 27 :| [])
             , (temp 46, occ 2 3 :| [])
             ]

        expState2 =
          let calleeSaveRange = occ 0 26 :| []
          in RegMap $ EnumMap.fromList
             [ (Reg Rax, occ 22 23 :| [])
             , (Reg Rbx, calleeSaveRange)
             , (Reg R12, calleeSaveRange)
             , (Reg R13, calleeSaveRange)
             , (Reg R14, calleeSaveRange)
             , (Reg R15, calleeSaveRange)
             , (Reg Rdi, occ 0 0 :| [])
             , (Reg Rsi, occ 0 1 :| [])
             , (temp 0, occ 0 20 :| [])
             , (temp 1, occ 1 18 :| [occ 24 25])
             , (temp 2, occ 2 2 :| [occ 20 22, occ 25 26])
             , (temp 8, occ 7 17 :| [])
             , (temp 9, occ 4 9 :| [])
             , (temp 10, occ 3 5 :| [])
             , (temp 11, occ 6 8 :| [])
             , (temp 12, occ 14 17 :| [])
             , (temp 13, occ 11 16 :| [])
             , (temp 14, occ 10 12 :| [])
             , (temp 15, occ 13 15 :| [])
             ]

        expState3 =
          let calleeSaveRange = occ 0 19 :| []
          in RegMap $ EnumMap.fromList
             [ (Reg Rax, occ 16 17 :| [])
             , (Reg Rdi, occ 0 0 :| [])
             , (Reg Rbx, calleeSaveRange)
             , (Reg R12, calleeSaveRange)
             , (Reg R13, calleeSaveRange)
             , (Reg R14, calleeSaveRange)
             , (Reg R15, calleeSaveRange)
             , (temp 0, occ 0 14 :| [occ 18 19])
             , (temp 1, occ 1 16 :| [occ 18 19])
             , (temp 2, occ 2 14 :| [occ 18 19])
             , (temp 3, occ 3 3 :| [occ 9 11])
             , (temp 31, occ 5 6 :| [])
             ]

        expState4 =
          let calleeSaveRange = occ 0 42 :| []
              callerSaveRanges = occ 4 4 :| [occ 8 8, occ 19 19]
          in RegMap $ EnumMap.fromList
             [ (Reg Rax, occ 4 5 :| [occ 8 9, occ 19 20, occ 41 42])
             , (Reg Rdi, occ 0 0 :| [occ 3 4, occ 7 8, occ 18 19])
             , (Reg Rsi, occ 0 4 :| [occ 6 8, occ 17 19])
             , (Reg Rsp, occ 0 39 :| [])
             , (Reg Rcx, callerSaveRanges)
             , (Reg Rdx, callerSaveRanges)
             , (Reg R8, callerSaveRanges)
             , (Reg R9, callerSaveRanges)
             , (Reg R10, callerSaveRanges)
             , (Reg R11, callerSaveRanges)
             , (Reg Rbx, calleeSaveRange)
             , (Reg R12, calleeSaveRange)
             , (Reg R13, calleeSaveRange)
             , (Reg R14, calleeSaveRange)
             , (Reg R15, calleeSaveRange)
             , (temp 0, occ 0 39 :| [])
             , (temp 1, occ 1 39 :| [])
             , (temp 2, occ 10 41 :| [])
             , (temp 3, occ 11 39 :| [])
             , (temp 4, occ 22 36 :| [])
             , (temp 5, occ 23 36 :| [])
             , (temp 6, occ 12 39 :| [])
             , (temp 7, occ 5 6 :| [])
             , (temp 8, occ 9 10 :| [])
             , (temp 9, occ 20 21 :| [])
             , (temp 10, occ 28 34 :| [])
             , (temp 11, occ 31 34 :| [])
             , (temp 12, occ 29 33 :| [])
             ]

        occ f t = RegState { stInterval = Interval f t, stFree = False }

testCFG1 :: ControlFlowGraph (Instr (TempReg Reg))
testCFG1 = ControlFlowGraph {..}
  where cfgGraph = Graph.mkGraph
          [(0, n0), (1, n1), (2, n2), (3, n3), (4, n4), (5, n5)]
          [ (0, 1, ()), (1, 5, ()), (1, 2, ()), (2, 5, ()), (2, 3, ())
          , (3, 5, ()), (3, 4, ()), (4, 1, ())
          ]
        cfgNodes = HashMap.fromList [ (LabelText "readLine", 0)
                                    , (LabelInt 10, 1)
                                    , (LabelInt 11, 2)
                                    , (LabelInt 7, 3)
                                    , (LabelInt 9, 4)
                                    , (LabelInt 5, 5)
                                    ]
        n0 = Block { blockLabel  = LabelText "readLine"
                   , blockNeighs = OneNeigh (LabelInt 10)
                   , blockStmts  = [ Mov (Register (temp 0)) (Name (LabelText "l4")) ]
                   }
        n1 = Block { blockLabel  = LabelInt 10
                   , blockNeighs = TwoNeighs (LabelInt 5) (LabelInt 11)
                   , blockStmts  =
                     [ Label (LabelInt 10)
                     , Mov (Register (temp 46)) (Const 1)
                     , Test (Register (temp 46)) (Register (temp 46))
                     , Jcc Amd64.Eq (LabelInt 5) (LabelInt 11)
                     ]
                   }
        n2 = Block { blockLabel = LabelInt 11
                   , blockNeighs = TwoNeighs (LabelInt 5) (LabelInt 7)
                   , blockStmts =
                     [ Label (LabelInt 11)
                     , Call (LabelText "getchar") (CallArgs []) callDefs
                     , Mov (Register (temp 33)) (Register (Reg Rax))
                     , Mov (Register (temp 1)) (Register (temp 33))
                     , Mov (Register (Reg Rsi)) (Name (LabelInt 6))
                     , Mov (Register (Reg Rdi)) (Register (temp 1))
                     , Call (LabelText "stringEqual") (CallArgs [Rdi, Rsi]) callDefs
                     , Mov (Register (temp 34)) (Register (Reg Rax))
                     , Test (Register (temp 34)) (Register (temp 34))
                     , Jcc Amd64.Ne (LabelInt 5) (LabelInt 7)
                     ]
                   }
        n3 = Block { blockLabel = LabelInt 7
                   , blockNeighs = TwoNeighs (LabelInt 5) (LabelInt 9)
                   , blockStmts =
                     [ Label (LabelInt 7)
                     , Mov (Register (Reg Rsi)) (Name (LabelInt 4))
                     , Mov (Register (Reg Rdi)) (Register (temp 1))
                     , Call (LabelText "stringEqual") (CallArgs [Rdi, Rsi]) callDefs
                     , Mov (Register (temp 35)) (Register (Reg Rax))
                     , Test (Register (temp 35)) (Register (temp 35))
                     , Jcc Amd64.Ne (LabelInt 5) (LabelInt 9)
                     ]
                   }
        n4 = Block { blockLabel = LabelInt 9
                   , blockNeighs = OneNeigh (LabelInt 10)
                   , blockStmts =
                     [ Label (LabelInt 9)
                     , Mov (Register (Reg Rsi)) (Register (temp 1))
                     , Mov (Register (Reg Rdi)) (Register (temp 0))
                     , Call (LabelText "concat") (CallArgs [Rdi, Rsi]) callDefs
                     , Mov (Register (temp 36)) (Register (Reg Rax))
                     , Mov (Register (temp 0)) (Register (temp 36))
                     , Jmp (LabelInt 10)
                     ]
                   }
        n5 = Block { blockLabel = LabelInt 5
                   , blockNeighs = ZeroNeighs
                   , blockStmts =
                     [ Label (LabelInt 5)
                     , Mov (Register (Reg Rax)) (Register (temp 0))
                     , Ret (Rax : calleeSaveRegisters)
                     ]
                   }

testCFG2 :: ControlFlowGraph (Instr (TempReg Reg))
testCFG2 = ControlFlowGraph {..}
  where cfgGraph = Graph.mkGraph [(0, n0), (1, n1), (2, n2), (3, n3)]
                                 [(0, 1, ()), (0, 3, ()), (1, 2, ()), (3, 2, ())]
        cfgNodes = HashMap.fromList [ (LabelText "maxVector", 0)
                                    , (LabelInt 1, 1)
                                    , (LabelInt 3, 2)
                                    , (LabelInt 2, 3)
                                    ]

        n0 = Block { blockLabel = LabelText "maxVector"
                   , blockNeighs = TwoNeighs (LabelInt 2) (LabelInt 1)
                   , blockStmts =
                     [ Mov (Register (temp 0)) (Register (Reg Rdi))
                     , Mov (Register (temp 1)) (Register (Reg Rsi))
                     , Xor (Register (temp 2)) (Register (temp 2))
                     , Mov (Register (temp 10)) (AddrRegBase (Offset 8) (Base (temp 0)))
                     , Mov (Register (temp 9)) (AddrRegBase (Offset 8) (Base (temp 0)))
                     , Imul (Imul2 (Register (temp 9)) (Register (temp 10)))
                     , Mov (Register (temp 11)) (AddrRegBase (Offset 0) (Base (temp 0)))
                     , Mov (Register (temp 8)) (AddrRegBase (Offset 0) (Base (temp 0)))
                     , Imul (Imul2 (Register (temp 8)) (Register (temp 11)))
                     , Lea (Register (temp 8)) (AddrRegBaseIndex (Offset 0)
                                                                 (Just (Base (temp 8)))
                                                                 (Index (temp 9))
                                                                 (Scale 1))
                     , Mov (Register (temp 14)) (AddrRegBase (Offset 8) (Base (temp 1)))
                     , Mov (Register (temp 13)) (AddrRegBase (Offset 8) (Base (temp 1)))
                     , Imul (Imul2 (Register (temp 13)) (Register (temp 14)))
                     , Mov (Register (temp 15)) (AddrRegBase (Offset 0) (Base (temp 1)))
                     , Mov (Register (temp 12)) (AddrRegBase (Offset 0) (Base (temp 1)))
                     , Imul (Imul2 (Register (temp 12)) (Register (temp 15)))
                     , Lea (Register (temp 12)) (AddrRegBaseIndex (Offset 0)
                                                                  (Just (Base (temp 12)))
                                                                  (Index (temp 13))
                                                                  (Scale 1))
                     , Cmp (Register (temp 8)) (Register (temp 12))
                     , Jcc Amd64.Lt (LabelInt 2) (LabelInt 1)
                     ]
                   }
        n1 = Block { blockLabel = LabelInt 1
                   , blockNeighs = OneNeigh (LabelInt 3)
                   , blockStmts =
                     [ Label (LabelInt 1)
                     , Mov (Register (temp 2)) (Register (temp 0))
                     ]
                   }

        n2 = Block { blockLabel = LabelInt 3
                   , blockNeighs = ZeroNeighs
                   , blockStmts =
                     [ Label (LabelInt 3)
                     , Mov (Register (Reg Rax)) (Register (temp 2))
                     , Ret (Rax : calleeSaveRegisters)
                     ]
                   }

        n3 = Block { blockLabel = LabelInt 2
                   , blockNeighs = OneNeigh (LabelInt 3)
                   , blockStmts =
                     [ Label (LabelInt 2)
                     , Mov (Register (temp 2)) (Register (temp 1))
                     , Jmp (LabelInt 3)
                     ]
                   }

testLoopCFG3 :: ControlFlowGraph (Instr (TempReg Reg))
testLoopCFG3 = ControlFlowGraph {..}
  where cfgGraph = Graph.mkGraph [(0, n0), (1, n1), (2, n2), (3, n3), (4, n4)]
                                 [ (0, 1, ()), (1, 3, ()), (1, 2, ()), (2, 4, ())
                                 , (2, 3, ()), (4, 1, ())
                                 ]
        cfgNodes = HashMap.fromList [ (LabelText "fib", 0)
                                    , (LabelInt 5, 1)
                                    , (LabelInt 6, 2)
                                    , (LabelInt 2, 3)
                                    , (LabelInt 4, 4)
                                    ]

        n0 = Block { blockLabel = LabelText "fib"
                   , blockNeighs = OneNeigh (LabelInt 5)
                   , blockStmts =
                     [ Mov (Register (temp 0)) (Register (Reg Rdi))
                     , Xor (Register (temp 1)) (Register (temp 1))
                     , Mov (Register (temp 2)) (Const 1)
                     , Xor (Register (temp 3)) (Register (temp 3))
                     ]
                   }

        n1 = Block { blockLabel = LabelInt 5
                   , blockNeighs = TwoNeighs (LabelInt 2) (LabelInt 6)
                   , blockStmts =
                     [ Label (LabelInt 5)
                     , Mov (Register (temp 31)) (Const 1)
                     , Test (Register (temp 31)) (Register (temp 31))
                     , Jcc Amd64.Eq (LabelInt 2) (LabelInt 6)
                     ]
                   }

        n2 = Block { blockLabel = LabelInt 6
                   , blockNeighs = TwoNeighs (LabelInt 4) (LabelInt 2)
                   , blockStmts =
                     [ Label (LabelInt 6)
                     , Lea (Register (temp 3)) (AddrRegBaseIndex (Offset 0)
                                                                 (Just (Base (temp 1)))
                                                                 (Index (temp 2))
                                                                 (Scale 1))
                     , Mov (Register (temp 1)) (Register (temp 2))
                     , Mov (Register (temp 2)) (Register (temp 3))
                     , Lea (Register (temp 0)) (AddrRegBase (Offset (-1)) (Base (temp 0)))
                     , Test (Register (temp 0)) (Register (temp 0))
                     , Jcc Amd64.Gt (LabelInt 4) (LabelInt 2)
                     ]
                   }

        n3 = Block { blockLabel = LabelInt 2
                   , blockNeighs = ZeroNeighs
                   , blockStmts =
                     [ Label (LabelInt 2)
                     , Mov (Register (Reg Rax)) (Register (temp 1))
                     , Ret (Rax : calleeSaveRegisters)
                     ]
                   }

        n4 = Block { blockLabel = LabelInt 4
                   , blockNeighs = OneNeigh (LabelInt 5)
                   , blockStmts =
                     [ Label (LabelInt 4)
                     , Jmp (LabelInt 5)
                     ]
                   }
             
testLoopCFG4 :: ControlFlowGraph (Instr (TempReg Reg))
testLoopCFG4 = ControlFlowGraph {..}
  where cfgGraph = Graph.mkGraph [ (0, n0), (1, n1), (2, n2), (3, n3), (4, n4), (5, n5)
                                 , (6, n6)
                                 ]
                                 [ (0, 1, ()), (1, 6, ()), (1, 2, ()), (2, 3, ())
                                 , (3, 5, ()), (3, 4, ()), (4, 3, ()), (5, 1, ())
                                 ]
        cfgNodes = HashMap.fromList [ (LabelText "addMat", 0)
                                    , (LabelInt 5, 1)
                                    , (LabelInt 6, 2)
                                    , (LabelInt 3, 3)
                                    , (LabelInt 4, 4)
                                    , (LabelInt 2, 5)
                                    , (LabelInt 1, 6)
                                    ]

        n0 = Block { blockLabel = LabelText "addMat"
                   , blockNeighs = OneNeigh (LabelInt 5)
                   , blockStmts =
                     [ Mov (Register (temp 0)) (Register (Reg Rdi))
                     , Mov (Register (temp 1)) (Register (Reg Rsi))
                     , Xor (Register (Reg Rsi)) (Register (Reg Rsi))
                     , Mov (Register (Reg Rdi)) (Const 3)
                     , Call (LabelText "createArray") (CallArgs [Rdi, Rsi]) callDefs
                     , Mov (Register (temp 7)) (Register (Reg Rax))
                     , Mov (Register (Reg Rsi)) (Register (temp 7))
                     , Mov (Register (Reg Rdi)) (Const 3)
                     , Call (LabelText "createArray") (CallArgs [Rdi, Rsi]) callDefs
                     , Mov (Register (temp 8)) (Register (Reg Rax))
                     , Mov (Register (temp 2)) (Register (temp 8))
                     , Xor (Register (temp 3)) (Register (temp 3))
                     , Mov (Register (temp 6)) (Const 2)
                     ]
                   }

        n1 = Block { blockLabel = LabelInt 5
                   , blockNeighs = TwoNeighs (LabelInt 1) (LabelInt 6)
                   , blockStmts =
                     [ Label (LabelInt 5)
                     , Cmp (Register (temp 3)) (Register (temp 6))
                     , Jcc Amd64.Gt (LabelInt 1) (LabelInt 6)
                     ]
                   }

        n2 = Block { blockLabel = LabelInt 6
                   , blockNeighs = OneNeigh (LabelInt 3)
                   , blockStmts =
                     [ Label (LabelInt 6)
                     , Xor (Register (Reg Rsi)) (Register (Reg Rsi))
                     , Mov (Register (Reg Rdi)) (Const 3)
                     , Call (LabelText "createArray") (CallArgs [Rdi, Rsi]) callDefs
                     , Mov (Register (temp 9)) (Register (Reg Rax))
                     , Mov (AddrRegBaseIndex (Offset 0)
                                             (Just (Base (temp 2)))
                                             (Index (temp 3))
                                             (Scale 8)) (Register (temp 9))
                     , Xor (Register (temp 4)) (Register (temp 4))
                     , Mov (Register (temp 5)) (Const 2)
                     ]
                   }

        n3 = Block { blockLabel = LabelInt 3
                   , blockNeighs = TwoNeighs (LabelInt 2) (LabelInt 4)
                   , blockStmts =
                     [ Label (LabelInt 3)
                     , Cmp (Register (temp 4)) (Register (temp 5))
                     , Jcc Amd64.Gt (LabelInt 2) (LabelInt 4)
                     ]
                   }

        n4 = Block { blockLabel = LabelInt 4
                   , blockNeighs = OneNeigh (LabelInt 3)
                   , blockStmts =
                     [ Label (LabelInt 4)
                     , Mov (Register (temp 10)) (AddrRegBaseIndex (Offset 0)
                                                                  (Just (Base (temp 2)))
                                                                  (Index (temp 3))
                                                                  (Scale 8))
                     , Mov (Register (temp 12)) (AddrRegBaseIndex (Offset 0)
                                                                  (Just (Base (temp 1)))
                                                                  (Index (temp 3))
                                                                  (Scale 8))
                     , Mov (Register (temp 12)) (AddrRegBaseIndex (Offset 0)
                                                                  (Just (Base (temp 12)))
                                                                  (Index (temp 4))
                                                                  (Scale 8))
                     , Mov (Register (temp 11)) (AddrRegBaseIndex (Offset 0)
                                                                  (Just (Base (temp 0)))
                                                                  (Index (temp 3))
                                                                  (Scale 8))
                     , Mov (Register (temp 11)) (AddrRegBaseIndex (Offset 0)
                                                                  (Just (Base (temp 11)))
                                                                  (Index (temp 4))
                                                                  (Scale 8))
                     , Lea (Register (temp 11)) (AddrRegBaseIndex (Offset 0)
                                                                  (Just (Base (temp 11)))
                                                                  (Index (temp 12))
                                                                  (Scale 1))
                     , Mov (AddrRegBaseIndex (Offset 0)
                                             (Just (Base (temp 10)))
                                             (Index (temp 4))
                                             (Scale 8)) (Register (temp 11))
                     , Lea (Register (temp 4)) (AddrRegBase (Offset 1) (Base (temp 4)))
                     , Jmp (LabelInt 3)
                     ]
                   }

        n5 = Block { blockLabel = LabelInt 2
                   , blockNeighs = OneNeigh (LabelInt 5)
                   , blockStmts =
                     [ Label (LabelInt 2)
                     , Lea (Register (temp 3)) (AddrRegBase (Offset 1) (Base (temp 3)))
                     , Jmp (LabelInt 5)
                     ]
                   }

        n6 = Block { blockLabel = LabelInt 1
                   , blockNeighs = ZeroNeighs
                   , blockStmts =
                     [ Label (LabelInt 1)
                     , Mov (Register (Reg Rax)) (Register (temp 2))
                     , Ret (Rax : calleeSaveRegisters)
                     ]
                   }

callDefs :: CallDefs
callDefs = CallDefs [Rax, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11]

calleeSaveRegisters :: [Reg]
calleeSaveRegisters = [Rbx, R12, R13, R14, R15]

temp :: Int -> TempReg Reg
temp = Temp . Temp.Temp
