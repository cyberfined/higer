module Tiger.IR.Types
    ( IRData(..)
    , LabeledString(..)
    , IRFunction(..)
    , ControlFlowGraph(..)
    , insertFirstCFGNode
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

import           Data.Bifunctor       (Bifunctor (..))
import           Data.Graph.Inductive (Gr, Node)
import           Data.HashMap.Strict  (HashMap)
import           Data.List.NonEmpty   (NonEmpty)
import           Data.Text            (Text)

import qualified Data.Graph.Inductive as Graph
import qualified Data.HashMap.Strict  as HashMap

import qualified Tiger.Temp           as Temp

data IRData b f = IRData
    { irStrings   :: ![LabeledString]
    , irFunctions :: ![IRFunction b f]
    } deriving Functor

instance Bifunctor IRData where
    bimap f g ir@IRData{..} = ir { irFunctions = map (bimap f g) irFunctions }

data LabeledString = LabeledString
    { lStringValue :: !Text
    , lStringLabel :: !Temp.Label
    }

data IRFunction b f = IRFunction
    { irFuncBody  :: !b
    , irFuncFrame :: !f
    } deriving Functor

instance Bifunctor IRFunction where
    bimap f g IRFunction{..} = IRFunction { irFuncBody  = f irFuncBody
                                          , irFuncFrame = g irFuncFrame
                                          }

data ControlFlowGraph s = ControlFlowGraph
    { cfgGraph :: !(Gr (Block s) ())
    , cfgNodes :: !(HashMap Temp.Label Node)
    }

instance Functor ControlFlowGraph where
    fmap f cfg@ControlFlowGraph{..} = cfg { cfgGraph = first (fmap f) cfgGraph }

insertFirstCFGNode :: ControlFlowGraph s -> [s] -> Temp.Label -> ControlFlowGraph s
insertFirstCFGNode cfg@ControlFlowGraph{..} stmts label
  | Just oldBlock <- mOldBlock =
      let nodesMap = HashMap.insert label 0
                   $ HashMap.insert (blockLabel oldBlock) newNodeId cfgNodes
      in ControlFlowGraph { cfgGraph = newCfgGraph
                          , cfgNodes = nodesMap
                          }
  | otherwise = cfg
  where insertNode (n@(nodeId, oldBlock):ns) nodes
          | nodeId == 0 =
              let newBlock = Block { blockStmts  = stmts
                                   , blockLabel  = label
                                   , blockNeighs = OneNeigh (blockLabel oldBlock)
                                   }
                  n1 = (0, newBlock)
                  n2 = (newNodeId, oldBlock)
              in (n1 : n2 : ns, Just oldBlock)
          | otherwise   = insertNode ns (n:nodes)
        insertNode _ nodes = (nodes, Nothing)

        newCfgGraph = Graph.mkGraph newNodes newEdges
        newEdges = (0, newNodeId, ())
                 : map ((\(x, y) -> (x, y, ())) . bimap subst subst) (Graph.edges cfgGraph)
        (newNodes, mOldBlock) = insertNode (Graph.labNodes cfgGraph) []

        subst nodeId
          | nodeId == 0 = newNodeId
          | otherwise   = nodeId
        newNodeId = snd (Graph.nodeRange cfgGraph) + 1

data Block s = Block
    { blockStmts  :: ![s]
    , blockLabel  :: !Temp.Label
    , blockNeighs :: !Neighs
    } deriving Functor

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
