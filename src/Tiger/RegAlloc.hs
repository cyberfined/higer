module Tiger.RegAlloc where

import           Data.Bifunctor             (first)
import           Data.Graph.Inductive       (Gr)
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import           Data.Maybe                 (fromJust, fromMaybe)
import           Debug.Trace                (trace)

import           Tiger.Codegen              (Destinations (..), Instruction (..),
                                             InstructionBuilder (..), Sources (..),
                                             TempReg (..), WithOperands (..))
import           Tiger.EnumMap              (EnumMap)
import           Tiger.EnumSet              ((\\))
import           Tiger.Frame                (Frame (..))
import           Tiger.IR                   (Block (..), ControlFlowGraph (..),
                                             IRFunction (..))
import           Tiger.Liveness             (InOut (..), livenessCFG)
import           Tiger.TextUtils            (TextBuildable (..), intercalate)

import qualified Data.Graph.Inductive       as Graph
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

import qualified Tiger.EnumMap              as EnumMap
import qualified Tiger.EnumSet              as EnumSet
import qualified Tiger.Temp                 as Temp

regAllocFunc :: ( Frame f
                , Instruction (Instr f) (TempReg (Reg f))
                , Instruction (Instr f) (Reg f)
                , InstructionBuilder b (Instr f) (Reg f)
                )
             => IRFunction (ControlFlowGraph ((Instr f) (TempReg (Reg f)))) f
             -> IRFunction (ControlFlowGraph ((Instr f) (Reg f))) f
regAllocFunc = undefined

newtype RegMap r = RegMap (EnumMap r (NonEmpty RegState)) deriving newtype Eq

instance (TextBuildable r, Enum r) => TextBuildable (RegMap r) where
    toTextBuilder (RegMap states) = "RegMap:\n" <> intercalate "\n" (map go $ EnumMap.toList states)
      where go (r, ints) =  toTextBuilder r <> ": "
                         <> intercalate ", " (map buildState $ NonEmpty.toList ints)
            buildState RegState{..} =
                let freeB = if stFree then "free" else "occupied"
                in "(" <> Builder.decimal (intFrom stInterval) <> ", "
                       <> Builder.decimal (intTo stInterval) <> ", "
                       <> freeB <> ")"

data RegState = RegState
    { stInterval :: !Interval
    , stFree     :: !Bool
    } deriving stock Eq

data Interval = Interval
    { intFrom :: !Int
    , intTo   :: !Int
    } deriving stock Eq

instance TextBuildable Interval where
    toTextBuilder Interval{..} =  "(" <> Builder.decimal intFrom <> ", "
                               <> Builder.decimal intTo <> ")"

showInOutGr :: (TextBuildable a, TextBuildable r, Enum r)
            => Gr (Block (a, InOut r, Int)) ()
            -> String
showInOutGr gr = LazyText.unpack $ Builder.toLazyText $ intercalate "\n\n" $ map go $ Graph.dfs' gr
  where go n =
            let (_, _, Block{..}, _) = fromJust $ fst $ Graph.match n gr
            in intercalate "\n" $ map goBlock blockStmts
        goBlock (instr, InOut{..}, idx) = 
            let idxB = (if idx < 10 then "0" else "") <> Builder.decimal idx
            in idxB <> ". " <> toTextBuilder instr <> " | in = " <> buildEnumSet inSet
                    <> ", out = " <> buildEnumSet outSet

        buildEnumSet = intercalate "," . map toTextBuilder . EnumSet.toList

liveRanges :: (WithOperands a (TempReg r), Enum r, Bounded r, Eq r, TextBuildable r, TextBuildable (a (TempReg r)))
           => ControlFlowGraph (a (TempReg r))
           -> RegMap (TempReg r)
liveRanges cfg = RegMap $ foldr go EnumMap.empty $ Graph.dfs' $ trace (showInOutGr cfgInOut) cfgInOut
  where cfgInOut = numInstr $ cfgGraph $ livenessCFG cfg

        go n regMap =
            let (_, _, Block{..}, _) = fromJust $ fst $ Graph.match n cfgInOut
                fromIdx = case blockStmts of
                    ((_, _, idx):_) -> idx
                    _               -> 0
                (regMap', toMap) = foldr goBlock (regMap, EnumMap.empty) blockStmts
                insertReg rm reg toIdx =
                    let regInt = Interval { intFrom = fromIdx, intTo = toIdx }
                    in if reg == Temp (Temp.Temp 0)
                          then trace (LazyText.unpack $ Builder.toLazyText $ toTextBuilder regInt) $ EnumMap.alter (insertInterval regInt) reg rm
                          else EnumMap.alter (insertInterval regInt) reg rm
            in EnumMap.foldlWithKey insertReg regMap' toMap

        goBlock (instr, InOut{..}, idx) (regMap, toMap) =
            let (Destinations dst, Sources src) = getOperands instr
                fromRegs = outSet \\ inSet `EnumSet.union`
                    (EnumSet.fromList dst \\ EnumSet.fromList src)
                toRegs = inSet
                toMap' = EnumSet.foldl (updateToMap idx) toMap toRegs
                regMap' = EnumSet.foldl (updateRegMap idx toMap') regMap fromRegs
            in (regMap', EnumMap.withoutKeys toMap' fromRegs)

        updateToMap idx toMap reg = EnumMap.alter (Just . fromMaybe idx) reg toMap

        updateRegMap fromIdx toMap regMap reg =
            let toIdx = fromMaybe fromIdx (EnumMap.lookup reg toMap)
                int = Interval fromIdx toIdx
            in if reg == Temp (Temp.Temp 0)
                  then trace (LazyText.unpack $ Builder.toLazyText $ toTextBuilder int) $ EnumMap.alter (insertInterval int) reg regMap
                  else EnumMap.alter (insertInterval int) reg regMap

        insertInterval int mStates = Just $ case mStates of
            Just (st@RegState{..} :| sts)
              | intTo int + 1 == intFrom stInterval ->
                  let mrgInt = Interval { intFrom = intFrom int, intTo = intTo stInterval }
                      mrgSt = RegState { stInterval = mrgInt, stFree = False }
                  in mrgSt :| sts
              | otherwise -> intSt :| st : sts
            Nothing       -> intSt :| []
          where intSt = RegState { stInterval = int, stFree = False }

numInstr :: Gr (Block (a, b)) () -> Gr (Block (a, b, Int)) ()
numInstr gr = fst $ foldl go (Graph.empty, 0) $ Graph.dfs' gr
  where go (gr', idx) n =
            let ((from, _, Block{..}, to), _) = first fromJust $ Graph.match n gr
                (is, idx') = first reverse $ foldl goInstr ([], idx) blockStmts
                block = Block { blockStmts  = is
                              , blockLabel  = blockLabel
                              , blockNeighs = blockNeighs
                              }
            in (Graph.insert (from, n, block, to) gr', idx')
        goInstr (is, idx) (a, b) = ((a, b, idx) : is, idx + 1)
