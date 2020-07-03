module EscapeAnalysis (
    runEscapeAnalysis
    ) where

import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Maybe(fromJust)
import Control.Monad(mapM, mapM_)
import qualified Data.Map as M
import Absyn

data EscapeState = EscapeState { escparams :: [M.Map String Bool]
                               , escvars :: [M.Map String Bool]
                               }

initEscapeState :: EscapeState
initEscapeState = EscapeState { escparams = [M.empty]
                              , escvars = [M.empty]
                              }

type EscapeStateT a = StateT EscapeState Identity a

runEscapeAnalysis :: Expr -> Expr
runEscapeAnalysis expr = runIdentity $ evalStateT (escapeAnalysis expr) initEscapeState

insertEscParam :: String -> EscapeStateT ()
insertEscParam par = modify (\st -> let (p:ps) = escparams st in st{escparams = (M.insert par False p):ps})

insertEscVar :: String -> EscapeStateT ()
insertEscVar var = modify (\st -> let (v:vs) = escvars st in st{escvars = (M.insert var False v):vs})

enterFun :: [(String, Type, Bool)] -> EscapeStateT ()
enterFun args = modify (\st -> st{escparams = M.empty:escparams st, escvars = M.empty:escvars st}) >>
    mapM_ (\(v,_,_) -> insertEscParam v) args

leaveFun :: EscapeStateT ()
leaveFun = modify (\st -> st{escparams = tail $ escparams st, escvars = tail $ escvars st})

enterLet :: EscapeStateT ()
enterLet = modify (\st -> st{escparams = M.empty:escparams st, escvars = M.empty:escvars st})

leaveLet :: EscapeStateT ()
leaveLet = modify (\st -> st{escparams = tail $ escparams st, escvars = tail $ escvars st})

escapeAnalysis :: Expr -> EscapeStateT Expr
escapeAnalysis expr = case expr of
    LVal sc lv -> escapeLVal lv >>= \lv' -> return (LVal sc lv')
    Seq sc exs -> escapeSeq exs >>= \exs' -> return (Seq sc exs')
    Neg sc expr -> escapeAnalysis expr >>= \expr' -> return (Neg sc expr')
    Call sc f exs -> escapeSeq exs >>= \exs' -> return (Call sc f exs')
    BinOp sc e1 op e2 -> escapeSeq [e1,e2] >>= \[e1', e2'] -> return (BinOp sc e1' op e2')
    Record sc r args -> escapeSeq (map snd args) >>= \exs' ->
        return (Record sc r (zip (map fst args) exs'))
    Array sc t e1 e2 -> escapeSeq [e1,e2] >>= \[e1',e2'] ->
        return (Array sc t e1' e2')
    Assign sc lv expr -> escapeLVal lv >>= \lv' ->
        escapeAnalysis expr >>= \expr' ->
            return (Assign sc lv' expr')
    If sc cond th mel -> case mel of
        Nothing -> escapeSeq [cond,th] >>= \[cond',th'] -> return (If sc cond' th' Nothing)
        Just el -> escapeSeq [cond,th,el] >>= \[cond',th',el'] -> return (If sc cond' th' (Just el'))
    While sc cond expr -> escapeSeq [cond,expr] >>= \[cond',expr'] -> return (While sc cond' expr')
    For sc v esc from to expr -> get >>= \st ->
        let oldV = M.lookup v (head $ escvars st)
        in insertEscVar v >> escapeSeq [from,to,expr] >>= \[from',to',expr'] ->
            get >>= \st' ->
                let (p':ps') = escparams st'
                    (v':vs') = escvars st'
                    esc' = fromJust $ M.lookup v v'
                    st'' = case oldV of
                        Nothing -> st'{ escparams = p':ps'
                                      , escvars = (M.delete v v'):vs'
                                      }
                        Just val -> st'{ escparams = p':ps'
                                       , escvars = (M.insert v val v'):vs'
                                       }
                in put st'' >>
                   return (For sc v esc' from' to' expr')
    Let sc decs expr -> enterLet >> mapM fStage decs >>= mapM sStage >>= \decs' ->
        escapeAnalysis expr >>= \expr' -> leaveLet >> return (Let sc decs' expr')
    expr -> return expr
  where fStage dec = case dec of
          t@(TypeDec{}) -> return t
          VarDec sc (TypedVar v t esc expr) -> insertEscVar v >> escapeAnalysis expr >>= \expr' ->
              return (VarDec sc (TypedVar v t esc expr'))
          FunDec sc (TypedFun f args t expr) -> enterFun args >>
              escapeAnalysis expr >>= \expr' ->
                  get >>= \st -> let escp = head $ escparams st
                                     args' = map (\(v,t,_) -> (v,t, fromJust $ M.lookup v escp)) args
                                 in leaveFun >>
                                    return (FunDec sc (TypedFun f args' t expr'))
        sStage dec = case dec of
            VarDec sc (TypedVar v t esc expr) -> get >>= \st ->
                return (VarDec sc (TypedVar v t (fromJust $ M.lookup v (head $ escvars st)) expr))
            dec -> return dec

escapeLVal :: LVal -> EscapeStateT LVal
escapeLVal lv = case lv of
    LId var -> setEscape var >> return lv
    LDot lv f -> escapeLVal lv >>= \lv' -> return (LDot lv' f)
    LArr lv expr -> escapeLVal lv >>= \lv' ->
        escapeAnalysis expr >>= \expr' -> 
            return (LArr lv' expr')

escapeSeq :: [Expr] -> EscapeStateT [Expr]
escapeSeq exs
  | length exs == 0 = return exs
  | otherwise = get >>= \bakSt ->
      mapM (\e -> escapeAnalysis e >>= \e' -> get >>= \st -> put bakSt >> return (e',st)) exs >>= \tups ->
          let exs' = map fst tups
              st' = foldr1 mergeStates (map snd tups)
          in put st' >> return exs'
  where mergeStates st1 st2 = EscapeState { escparams = zipWith (M.unionWith (||)) (escparams st1) (escparams st2)
                                          , escvars = zipWith (M.unionWith (||)) (escvars st1) (escvars st2)
                                          }

setEscape :: String -> EscapeStateT ()
setEscape var = get >>= \st ->
    let ps = escparams st
        vs = escvars st
        (p:ps') = ps
        (v:vs') = vs
    in if length ps <= 1
          then return ()
          else case (M.lookup var v, M.lookup var p) of
              (Just _, _) -> return ()
              (_, Just _) -> return ()
              _ -> let (ps'', vs'') = setEscape' var ps' vs'
                   in put (EscapeState (p:ps'') (v:vs''))
  where setEscape' var (p:ps) (v:vs) = case (M.lookup var v, M.lookup var p) of
          (Just _, _) -> (p:ps, (M.insert var True v):vs)
          (_, Just _) -> ((M.insert var True p):ps, v:vs)
          _ -> let (ps', vs') = setEscape' var ps vs in ((p:ps'), (v:vs'))
