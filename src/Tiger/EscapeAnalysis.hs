{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Tiger.EscapeAnalysis (escapeAnalysis) where

import Control.Monad.State
import Data.Fix
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromJust)
import Data.Text hiding (tail, head, zipWith, map, foldr1)

import Tiger.Expr

import qualified Data.HashMap.Strict as HashMap

data Env = Env
    { envVars :: HashMap Text Escaping
    , envType :: EnvType
    }

data EnvType
    = LetEnv
    | FunEnv

escapeAnalysis :: Expr TypedDec -> Expr TypedDec
escapeAnalysis expr =
    runIdentity (evalStateT (foldFix escExprF expr) [Env HashMap.empty LetEnv])

withNewEnv :: MonadState [Env] m => EnvType -> m a -> m a
withNewEnv isFun ma = do
    modify (\es -> Env HashMap.empty isFun:es)
    res <- ma
    modify tail
    return res

lookupVar :: MonadState [Env] m => Text -> m (Maybe Escaping)
lookupVar var = gets (HashMap.lookup var . envVars . head)

insertVar :: MonadState [Env] m => Text -> Escaping -> m ()
insertVar var esc = modify $ \(env:es) ->
    let newEnv = env { envVars = HashMap.insert var esc $ envVars env }
    in newEnv:es

delVar :: MonadState [Env] m => Text -> m ()
delVar var = modify $ \(env:es) ->
    let newEnv = env { envVars = HashMap.delete var $ envVars env }
    in newEnv:es

setVar :: MonadState [Env] m => Text -> m ()
setVar var = modify (go Remaining)
  where go :: Escaping -> [Env] -> [Env]
        go isEsc (env@(Env vars isFun):es)
          | HashMap.member var vars
          = case isEsc of
              Escaping ->
                  let newEnv = env { envVars = HashMap.insert var isEsc vars }
                  in newEnv:es
              Remaining -> env:es
          | otherwise = env:go (escapeFromEnv isEsc isFun) es
        go _ _ = []

escExprF :: MonadState [Env] m => ExprF TypedDec (m (Expr TypedDec)) -> m (Expr TypedDec)
escExprF = \case
    LVal lv          -> mkLVal <$> escLValF lv
    Assign lval rval -> mkAssign <$> escLValF lval <*> rval
    For var _ mfrom mto mbody -> do
        bakVar <- lookupVar var
        insertVar var Remaining
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
            union env1 env2 = Env
                { envVars = HashMap.unionWith combineEscaping
                                              (envVars env1)
                                              (envVars env2)
                , envType = envType env1
                }
        put newSt
        return (mkSeq es)
    expr -> Fix <$> sequenceA expr

escLValF :: MonadState [Env] m
         => LValF TypedDec (m (Expr TypedDec))
         -> m (LValF TypedDec (Expr TypedDec))
escLValF = \case
    Var var -> setVar var >> return (Var var)
    Index mlv mind -> do
        lv <- mlv
        ind <- unFix <$> escExprF mind
        return (Index lv ind)
    lv -> sequenceA lv

escLet :: MonadState [Env] m
       => [TypedDec (m (Expr TypedDec))]
       -> m (Expr TypedDec)
       -> m (Expr TypedDec)
escLet decs mexpr = withNewEnv LetEnv $ do
    decs' <- mapM firstPass decs
    expr <- mexpr
    decs'' <- mapM secondPass decs'
    return (mkLet decs'' expr)
      where firstPass :: MonadState [Env] m
                      => TypedDec (m (Expr TypedDec))
                      -> m (TypedDec (Expr TypedDec))
            firstPass = \case
                TypedVarDec (TypedVar var typ esc expr) -> do
                    insertVar var Remaining
                    mkTypedVar var typ esc <$> expr
                TypedFunDec (TypedFun fn args retTyp mbody) -> withNewEnv FunEnv $ do
                    mapM_ (\arg -> insertVar (tFunArgName arg) Remaining) args
                    body <- mbody
                    args' <- forM args $ \arg -> do
                        esc <- fromJust <$> lookupVar (tFunArgName arg)
                        return arg { tFunArgEscaping = esc }
                    return (mkTypedFun fn args' retTyp body)
                dec -> sequenceA dec
            secondPass :: MonadState [Env] m
                       => TypedDec (Expr TypedDec)
                       -> m (TypedDec (Expr TypedDec))
            secondPass = \case
                TypedVarDec (TypedVar var typ _ expr) -> do
                    esc <- fromJust <$> lookupVar var
                    return (mkTypedVar var typ esc expr)
                dec -> return dec

combineEscaping :: Escaping -> Escaping -> Escaping
combineEscaping Escaping _ = Escaping
combineEscaping _ Escaping = Escaping
combineEscaping _ _        = Remaining

escapeFromEnv :: Escaping -> EnvType -> Escaping
escapeFromEnv Escaping _ = Escaping
escapeFromEnv _ FunEnv   = Escaping
escapeFromEnv _ _        = Remaining
