module Optim (
    runOptim
    ) where

import Optim.Subexpr
import Optim.SideEffectAnalysis
import Control.Monad((>=>), mapM, foldM)
import IR

runOptim :: IRState -> Either String IRState
runOptim = subExprElim . runSideEffectAnalysis
