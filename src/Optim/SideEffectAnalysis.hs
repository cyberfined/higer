module Optim.SideEffectAnalysis (
    runSideEffectAnalysis
    ) where

import qualified Data.Set as S
import qualified LibFuncs as LF
import Data.Graph
import Data.Foldable(foldl')
import IR

runSideEffectAnalysis :: IRState -> IRState
runSideEffectAnalysis irst = irst{sideEffectFuncs = se'}
  where (se, nse) = foldl' analyseFunc (S.fromList LF.sideEffectFuncs, S.fromList LF.pureFuncs) $ funDecs irst
        (se', _) = foldl' analyseFunc (se, nse) $ funDecs irst

analyseFunc :: (S.Set String, S.Set String) -> FunDec -> (S.Set String, S.Set String)
analyseFunc (se, nse) fd = analyseFunc' False $ map (node (nodes fd)) $ vertices $ cfg fd
  where analyseFunc' isUndef ((ir:irs):bs) = case ir of
            Load _ -> seRes
            Store{} -> seRes
            RuntimeCall _ -> seRes
            Call f _ | S.member f se -> seRes
                     | S.member f nse -> analyseFunc' isUndef (irs:bs)
                     | otherwise -> analyseFunc' True (irs:bs)
            _ -> analyseFunc' isUndef (irs:bs)
          where seRes = (S.insert (name fd) se, nse)
        analyseFunc' isUndef (_:bs) = analyseFunc' isUndef bs
        analyseFunc' isUndef _ = if isUndef
                                    then (se, nse)
                                    else (se, S.insert (name fd) nse)
