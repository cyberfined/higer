module Tiger.Codegen
    ( module Tiger.Codegen.Assem
    , codegen
    ) where

import           Control.Monad        (forM)
import           Data.Foldable        (fold)
import           Data.Graph.Inductive (Graph (..))

import           Tiger.Codegen.Assem
import           Tiger.Frame          (Frame, Instr, Reg)
import           Tiger.IR.Types       (Block (..), ControlFlowGraph (..), IRData (..),
                                       IRFunction (..), Stmt)
import           Tiger.Temp           (MonadTemp)

import qualified Tiger.DList          as DList
import qualified Tiger.Frame          as Frame

codegen :: ( MonadTemp m
           , Frame f
           , Instruction (Instr f) (TempReg (Reg f))
           , Instruction (Instr f) (Reg f)
           )
        => IRData (ControlFlowGraph Stmt) f
        -> m (IRData (ControlFlowGraph (Instr f (TempReg (Reg f)))) f)
codegen ir@IRData{..} = do
    funcs <- forM irFunctions $ \func@IRFunction{..} -> do
        let nodes = labNodes (cfgGraph irFuncBody)
        let edges = labEdges (cfgGraph irFuncBody)
        nodes' <- forM nodes $ \(node, block@Block{..}) -> do
            is <- DList.toList . fold <$> mapM (Frame.codegen irFuncFrame) blockStmts
            pure (node, block { blockStmts = is })
        let cfg = irFuncBody { cfgGraph = mkGraph nodes' edges }
        pure $ func { irFuncBody = cfg }
    pure $ ir { irFunctions = funcs }
