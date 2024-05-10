module Tiger.IR.Types
    ( IRData(..)
    , LabeledString(..)
    , IRFunction(..)
    , IR(..)
    , Expr(..)
    , Stmt(..)
    , Cond
    , Binop(..)
    , Relop(..)
    ) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text          (Text)

import qualified Tiger.Temp         as Temp

data IRData f = IRData
    { irStrings   :: ![LabeledString]
    , irFunctions :: ![IRFunction f]
    }

data LabeledString = LabeledString
    { lStringValue :: !Text
    , lStringLabel :: !Temp.Label
    }

data IRFunction f = IRFunction
    { irFuncBody  :: !Stmt
    , irFuncFrame :: !f
    }

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
