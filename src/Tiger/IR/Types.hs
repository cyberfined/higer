module Tiger.IR.Types
    ( IR(..)
    , Operand
    , Stmt
    , Binop(..)
    , Relop(..)
    , IRData(..)
    , LabeledString(..)
    , IRFunction(..)
    ) where

import           Data.Text  (Text)

import qualified Tiger.Temp as Temp

data IRData f = IRData
    { resStrings   :: ![LabeledString]
    , resFunctions :: ![IRFunction f]
    }

data LabeledString = LabeledString
    { lStringValue :: !Text
    , lStringLabel :: !Temp.Label
    }

data IRFunction f = IRFunction
    { irFuncBody  :: ![IR Stmt]
    , irFuncFrame :: !f
    }

data IR a where
    Const  :: !Int -> IR Operand
    Name   :: !Temp.Label -> IR Operand
    Temp   :: !Temp.Temp -> IR Operand
    Assign :: !Temp.Temp -> !(IR Operand) -> IR Stmt
    Binop  :: !Temp.Temp -> !Binop -> !(IR Operand) -> !(IR Operand) -> IR Stmt
    Load   :: !Temp.Temp -> !(IR Operand) -> IR Stmt
    Store  :: !(IR Operand) -> !Temp.Temp -> IR Stmt
    Label  :: !Temp.Label -> IR Stmt
    Jump   :: !Temp.Label -> IR Stmt
    CJump  :: !Relop
           -> !(IR Operand)
           -> !(IR Operand)
           -> !Temp.Label
           -> !Temp.Label
           -> IR Stmt
    Call   :: !(Maybe Temp.Temp) -> !Temp.Label -> ![IR Operand] -> IR Stmt
    Ret    :: !(Maybe (IR Operand)) -> IR Stmt

data Operand

data Stmt

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
    | ULt
    | ULe
    | UGt
    | UGe
