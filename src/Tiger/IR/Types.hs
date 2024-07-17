module Tiger.IR.Types
    ( IRDataStmt
    , IRDataCFG
    , IRData(..)
    , LabeledString(..)
    , IRFunctionStmt
    , IRFunctionCFG
    , IRFunction(..)
    , ControlFlowGraph(..)
    , Block(..)
    , Neighs(..)
    , removeNeigh
    , IR(..)
    , Expr(..)
    , Stmt(..)
    , Cond
    , Binop(..)
    , Relop(..)
    , notRelop
    ) where

import           Data.Graph.Inductive (Gr, Node)
import           Data.HashMap.Strict  (HashMap)
import           Data.List.NonEmpty   (NonEmpty)
import           Data.Text            (Text)

import qualified Tiger.Temp           as Temp

type IRDataStmt f = IRData Stmt f

type IRDataCFG f = IRData ControlFlowGraph f

data IRData b f = IRData
    { irStrings   :: ![LabeledString]
    , irFunctions :: ![IRFunction b f]
    }

data LabeledString = LabeledString
    { lStringValue :: !Text
    , lStringLabel :: !Temp.Label
    }

type IRFunctionStmt f = IRFunction Stmt f

type IRFunctionCFG f = IRFunction ControlFlowGraph f

data IRFunction b f = IRFunction
    { irFuncBody  :: !b
    , irFuncFrame :: !f
    }

data ControlFlowGraph = ControlFlowGraph
    { cfgGraph :: !(Gr Block ())
    , cfgNodes :: !(HashMap Temp.Label Node)
    }

data Block = Block
    { blockStmts  :: ![Stmt]
    , blockLabel  :: !Temp.Label
    , blockNeighs :: !Neighs
    }

data Neighs
    = ZeroNeighs
    | OneNeigh !Temp.Label
    | TwoNeighs !Temp.Label !Temp.Label

removeNeigh :: Temp.Label -> Neighs -> Neighs
removeNeigh n = \case
    ZeroNeighs    -> ZeroNeighs
    OneNeigh n1
      | n1 == n   -> ZeroNeighs
      | otherwise -> OneNeigh n1
    TwoNeighs n1 n2
      | n1 == n   -> OneNeigh n2
      | n2 == n   -> OneNeigh n1
      | otherwise -> TwoNeighs n1 n2

data IR
    = Ex !Expr
    | Nx !Stmt
    | Cx !Cond

data Expr
    = Const !Int
    | Name !Temp.Label
    | Temp !Temp.Temp
    | Binop !Binop !Expr !Expr
    | Mem !Expr
    | Call !Temp.Label ![Expr]
    | ESeq !Stmt !Expr

data Stmt
    = Move !Expr !Expr
    | Expr !Expr
    | Jump !Temp.Label
    | CJump !Relop !Expr !Expr !Temp.Label !Temp.Label
    | Seq !(NonEmpty Stmt)
    | Label !Temp.Label
    | Ret

type Cond = Temp.Label -> Temp.Label -> Stmt

data Binop
    = Add
    | Sub
    | Mul
    | Div
    | And
    | Or
    | Xor
    | LShift
    | RShift

data Relop
    = Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge

notRelop :: Relop -> Relop
notRelop = \case
    Eq -> Ne
    Ne -> Eq
    Lt -> Ge
    Le -> Gt
    Gt -> Le
    Ge -> Lt
