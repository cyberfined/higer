module Tiger.Liveness
    ( InOut(..)
    , livenessCFG
    ) where

import           Data.Bifunctor       (Bifunctor (..))
import           Data.Graph.Inductive (Gr)
import           Data.Maybe           (fromJust)

import           Tiger.Codegen        (Destinations (..), Sources (..), WithOperands (..))
import           Tiger.EnumSet        (EnumSet, (\\))
import           Tiger.IR             (Block (..), ControlFlowGraph (..))
import           Tiger.ListUtils      (unsnoc)

import qualified Data.Graph.Inductive as Graph

import qualified Tiger.EnumSet        as EnumSet

data UseDef r = UseDef
    { useSet :: !(EnumSet r)
    , defSet :: !(EnumSet r)
    }

data InOut r = InOut
    { inSet  :: !(EnumSet r)
    , outSet :: !(EnumSet r)
    } deriving stock Eq

livenessCFG :: WithOperands a r
            => ControlFlowGraph (a r)
            -> ControlFlowGraph (a r, InOut r)
livenessCFG ControlFlowGraph{..} = ControlFlowGraph { cfgGraph = inOutCfg
                                                    , cfgNodes = cfgNodes
                                                    }
  where inOutCfg = first (\(b, _, io) -> addInstrInOut b io) $ iter useDefCFG
        useDefCFG = addUseDefToCFG cfgGraph
        order = Graph.dfs' useDefCFG

        iter gr =
            let (newGr, ch) = foldr go (gr, False) order
            in if ch then iter newGr else newGr

        calcOutSet gr set (_, n) =
            let (_, _, (_, _, InOut{..}), _) = fromJust $ fst $ Graph.match n gr
            in set `EnumSet.union` inSet

        inOutChanged :: Enum a => InOut a -> InOut a -> Bool
        inOutChanged io1 io2 =  EnumSet.size (inSet io1) /= EnumSet.size (inSet io2)
                             || EnumSet.size (outSet io1) /= EnumSet.size (outSet io2)

        go n (newGr, ch) =
            let ((from, _, curNode, to), gr') = first fromJust (Graph.match n newGr)
                (block, useDef@UseDef{..}, inOut) = curNode
                outSet' = foldl (calcOutSet newGr) EnumSet.empty to
                inSet' = useSet `EnumSet.union` (outSet' \\ defSet)
                inOut' = InOut { inSet = inSet', outSet = outSet' }
                newGr' = Graph.insert (from, n, (block, useDef, inOut'), to) gr'
            in if inOutChanged inOut inOut' then (newGr', True) else (newGr, ch)

addInstrInOut :: WithOperands a r => Block (a r) -> InOut r -> Block (a r, InOut r)
addInstrInOut Block{..} inOut = Block { blockStmts  = stmts
                                      , blockLabel  = blockLabel
                                      , blockNeighs = blockNeighs
                                      }
  where stmts = case unsnoc blockStmts of
            Just (xs, instr) ->
                let lastIstrInOut = instrInOut instr (outSet inOut)
                in fst $ foldr go ([(instr, lastIstrInOut)], lastIstrInOut) xs
            _ -> []

        go instr (is, InOut{..}) =
            let curInstrInOut = instrInOut instr inSet
            in ((instr, curInstrInOut) : is, curInstrInOut)

        instrInOut instr instrOut =
            let operands = getOperands instr
                instrDef = EnumSet.fromList $ getDestination $ fst operands
                instrUse = EnumSet.fromList $ getSource $ snd operands
                instrIn = instrUse `EnumSet.union` (instrOut \\ instrDef)
            in InOut { inSet = instrIn, outSet = instrOut }

addUseDefToCFG :: WithOperands a r
               => Gr (Block (a r)) ()
               -> Gr (Block (a r), UseDef r, InOut r) ()
addUseDefToCFG = first (\b -> (b, blockUseDef b, emptyInOut))
  where emptyInOut = InOut EnumSet.empty EnumSet.empty

blockUseDef :: WithOperands a r => Block (a r) -> UseDef r
blockUseDef Block{..} = foldr combUseDef emptyUseDef blockStmts
  where emptyUseDef = UseDef { useSet = EnumSet.empty, defSet = EnumSet.empty }
        combUseDef p UseDef{..} =
            let (Destinations dsts, Sources srcs) = getOperands p
                useP = EnumSet.fromList srcs
                defP = EnumSet.fromList dsts
                usePn = useP `EnumSet.union` (useSet \\ defP)
                defPn = defP `EnumSet.union` defSet
            in UseDef { useSet = usePn, defSet = defPn }
