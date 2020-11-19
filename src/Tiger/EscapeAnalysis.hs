{-# LANGUAGE FlexibleContexts #-}
module Tiger.EscapeAnalysis (escapeAnalysis) where

import Data.Fix
import Tiger.Expr

import Data.Text hiding(tail, head, zipWith, map, foldr1)
import Data.Maybe(fromJust)
import qualified Data.HashMap.Strict as HM

import Data.Functor.Identity
import Control.Monad.State

type Env = HM.HashMap Text Bool

escapeAnalysis :: Expr TypedDec -> Expr TypedDec
escapeAnalysis expr = runIdentity (evalStateT (foldFix escExprF expr) [(HM.empty, False)])

pushEnv :: MonadState [(Env, Bool)] m => Bool -> m ()
pushEnv isFn = modify (\es -> (HM.empty, isFn):es)

popEnv :: MonadState [(Env, Bool)] m => m ()
popEnv = modify tail

lookupVar :: MonadState [(Env, Bool)] m => Text -> m (Maybe Bool)
lookupVar var = gets (HM.lookup var . fst . head)

insertVar :: MonadState [(Env, Bool)] m => Text -> Bool -> m ()
insertVar var val = modify (\((env,isFn):es) -> (HM.insert var val env, isFn):es)

delVar :: MonadState [(Env, Bool)] m => Text -> m ()
delVar var = modify (\((env,isFn):es) -> (HM.delete var env, isFn):es)

setVar :: MonadState [(Env, Bool)] m => Text -> m ()
setVar var = modify (go False)
  where go isEsc (e@(env, isFn):es)
          | HM.member var env = (if isEsc then (HM.insert var isEsc env, isFn) else e):es
          | otherwise = e:go (isEsc || isFn) es
        go _ _ = []

escExprF :: MonadState [(Env, Bool)] m
         => ExprF TypedDec (m (Expr TypedDec))
         -> m (Expr TypedDec)
escExprF expr = case expr of
    LVal lv -> mkLVal <$> escLValF lv
    Assign lval rval -> mkAssign <$> escLValF lval <*> rval
    For var _ mfrom mto mbody -> do
        bakVar <- lookupVar var
        insertVar var False
        from <- mfrom
        to <- mto
        body <- mbody
        esc <- fromJust <$> lookupVar var
        maybe (delVar var) (insertVar var) bakVar
        return (mkFor var esc from to body)
    Let decs mexpr -> escLet decs mexpr
    Seq mes -> do
        bakSt <- get
        tups <- forM mes $ \mexpr -> do
            expr <- mexpr
            st <- get
            put bakSt
            return (expr, st)
        let es = map fst tups
            newSt = foldr1 (zipWith union) (map snd tups)
            union (e1, isFn) (e2,_) = (HM.unionWith (||) e1 e2, isFn)
        put newSt
        return (mkSeq es)
    expr -> Fix <$> sequenceA expr

escLValF :: MonadState [(Env, Bool)] m
         => LValF TypedDec (m (Expr TypedDec))
         -> m (LValF TypedDec (Expr TypedDec))
escLValF lv = case lv of
    Var var -> setVar var >> return (Var var)
    Index mlv mind -> do
        lv <- mlv
        ind <- unFix <$> escExprF mind
        return (Index lv ind)
    lv -> sequenceA lv

escLet :: MonadState [(Env, Bool)] m
       => [TypedDec (m (Expr TypedDec))]
       -> m (Expr TypedDec)
       -> m (Expr TypedDec)
escLet decs mexpr = do
    pushEnv False
    decs' <- mapM fPass decs
    expr <- mexpr
    decs'' <- mapM sPass decs'
    popEnv
    return (mkLet decs'' expr)
  where fPass dec = case dec of
            TypedVar var typ esc expr -> do
                insertVar var False
                TypedVar var typ esc <$> expr
            TypedFun fn args retTyp mexpr -> do
                pushEnv True
                mapM_ (\(a,_,_) -> insertVar a False) args
                expr <- mexpr
                args' <- forM args $ \(a,t,_) -> do
                    esc <- fromJust <$> lookupVar a
                    return (a, t, esc)
                popEnv
                return (TypedFun fn args' retTyp expr)
            dec -> sequenceA dec
        sPass dec = case dec of
            TypedVar var typ _ expr -> do
                esc <- fromJust <$> lookupVar var
                return (TypedVar var typ esc expr)
            dec -> return dec
