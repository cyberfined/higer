{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiger.TypeCheck where

import Tiger.Utils
import Tiger.Expr
import Data.Fix

import Data.Text hiding ( length
                        , head
                        , tail
                        , last
                        , map
                        , zip
                        , zipWith
                        , null
                        , filter
                        )
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Maybe(listToMaybe, mapMaybe, isNothing)

import Data.Functor.Compose
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

data Env = Env { venv :: HM.HashMap Text Type
               , tenv :: HM.HashMap Text Type
               }

data CheckContext = CheckContext { inLoop :: Bool
                                 , sourceCode :: Text
                                 }

data CheckState = CheckState { envs :: [Env]
                             , curPos :: SrcSpan
                             }

getInLoop :: MonadReader CheckContext m => m Bool
getInLoop = reader inLoop

lookupVar :: MonadState CheckState m => Text -> m (Maybe Type)
lookupVar var = gets (listToMaybe . mapMaybe (HM.lookup var . venv) . envs)

lookupFun :: MonadState CheckState m => Text -> m (Maybe Type)
lookupFun = lookupVar

lookupType :: MonadState CheckState m => Text -> m (Maybe Type)
lookupType typ = gets (listToMaybe . mapMaybe (HM.lookup typ . tenv) . envs)

lookupTypeErr :: (MonadReader CheckContext m, MonadState CheckState m, MonadError Text m)
              => Text
              -> m Type
lookupTypeErr tid = lookupType tid >>= maybe (prettyError $ "undefined type " <> tid) return

insertVar :: MonadState CheckState m => Text -> Type -> m ()
insertVar var typ = modify $ \st ->
    let (e:es) = envs st
    in st{envs = e{venv = HM.insert var typ $ venv e}:es}

insertFun :: MonadState CheckState m => Text -> Type -> m ()
insertFun = insertVar

insertType :: MonadState CheckState m => Text -> Type -> m ()
insertType tid typ = modify $ \st ->
    let (e:es) = envs st
    in st{envs = e{tenv = HM.insert tid typ $ tenv e}:es}

deleteVar :: MonadState CheckState m => Text -> m ()
deleteVar var = modify $ \st ->
    let (e:es) = envs st
    in st{envs = e{venv = HM.delete var $ venv e}:es}

pushEnv :: MonadState CheckState m => m ()
pushEnv = modify (\st -> st{envs = Env HM.empty HM.empty:envs st})

popEnv :: MonadState CheckState m => m ()
popEnv = modify (\st -> st{envs = tail $ envs st})

prettyError :: (MonadReader CheckContext m, MonadState CheckState m, MonadError Text m)
            => Text
            -> m a
prettyError err = do
    SourcePos fpath ln cl <- gets (spanBegin . curPos)
    src <- reader sourceCode
    let strLn = showText ln
        margin = T.replicate (T.length strLn + 2) " "
        errMsg = pack fpath <> ":" <> strLn <> ":" <> showText cl <> ": error:\n" <> err <> "\n"
        line = T.takeWhile (/='\n') . (!!(ln-1)) . iterate (T.tail . T.dropWhile (/= '\n')) $ src
        prettyLine = margin <> "|\n " <> strLn <> " | " <> line <> "\n" <> margin <> "|\n"
    throwError $ errMsg <> prettyLine

typeCheck :: Text -> PosExpr UntypedDec -> Either Text (Expr TypedDec)
typeCheck src exp = runIdentity (runExceptT (runReaderT (evalStateT (run exp) initSt) initCtx))
  where run = fmap snd . adi (typeCheckExprF . annotated . getCompose) setContext
        initSt = CheckState { envs = initEnvs
                            , curPos = SrcSpan iniPos iniPos
                            }
        initCtx = CheckContext { inLoop = False
                               , sourceCode = src
                               }
        iniPos = SourcePos "" 1 1
        initEnvs = [ Env { tenv = HM.fromList [ ("int", TInt)
                                              , ("string", TString)
                                              ]
                          , venv = HM.fromList [ ("print", TFunc [TString] TUnit)
                                               , ("flush", TFunc [] TUnit)
                                               , ("getchar", TFunc [] TString)
                                               , ("ord", TFunc [TString] TInt)
                                               , ("chr", TFunc [TInt] TString)
                                               , ("size", TFunc [TString] TInt)
                                               , ("substring", TFunc [TString, TInt, TInt] TString)
                                               , ("concat", TFunc [TString, TString] TString)
                                               , ("not", TFunc [TInt] TInt)
                                               , ("exit", TFunc [TInt] TUnit)
                                               ]
                          }
                    ]

setContext :: (MonadReader CheckContext m, MonadState CheckState m, MonadError Text m)
           => (PosExpr UntypedDec -> m (Type, Expr TypedDec))
           -> PosExpr UntypedDec
           -> m (Type, Expr TypedDec)
setContext f exp = case exp of
    AnnE ann For{} -> loop ann
    AnnE ann While{} -> loop ann
    exp -> do
        modify (\st -> st{curPos = annotation $ getCompose $ unFix exp})
        f exp
  where loop ann =
            local (\st -> st{inLoop = True}) $ do
                modify (\st -> st{curPos = ann})
                f exp

typeCheckExprF :: (MonadReader CheckContext m, MonadState CheckState m, MonadError Text m)
               => ExprF UntypedDec (m (Type, Expr TypedDec))
               -> m (Type, Expr TypedDec)
typeCheckExprF expr = case expr of
    IntLit i -> return (TInt, mkIntLit i)
    StrLit s -> return (TString, mkStrLit s)
    Nil -> return (TNil, mkNil)
    LVal lv -> fmap mkLVal <$> typeCheckLvalF lv
    Neg mexp -> do
        (expTyp, exp) <- mexp
        when (expTyp /= TInt) $
            prettyError $  "type mismatch in neg expression: expected int but given "
                        <> showType expTyp
        return (TInt, mkNeg exp)
    Binop me1 op me2 -> do
        (typ1, e1) <- me1
        (typ2, e2) <- me2
        case op of
            Eq -> eqne typ1 typ2
            Ne -> eqne typ1 typ2
            _ -> do
                when (typ1 /= TInt) $ opError typ1
                when (typ2 /= TInt) $ opError typ2
        return (TInt, mkBinop op e1 e2)
      where eqne typ1 typ2 = when (isInvalidAssign typ1 typ2) $
                prettyError $  "type mismatch in " <> showBinop op
                            <> " operation: trying to compare " <> showType typ1
                            <> " with " <> showType typ2
            opError typ = prettyError $  "type mismatch in " <> showBinop op
                                      <> " operation: expected int but given "
                                      <> showType typ
    Record tid mfs -> do
        mtyp <- lookupType tid
        case mtyp of
            Nothing -> prettyError $ "undefined type " <> tid
            Just recTyp@(TRecord tid rfs) -> do
                let mdup = getDups (map fst mfs)
                maybe (return ()) (prettyError . ("repeated assignment to record field " <>)) mdup
                when (length mfs /= length rfs) $
                    prettyError "not all fields of record type are initialized"
                typedFs <- forM mfs $ \(field, mexp) -> do
                    (expTyp, exp) <- mexp
                    case lookup field rfs of
                        Nothing -> prettyError $  "record type " <> tid
                                               <> " have no field " <> field
                        Just fTyp
                          | isInvalidAssign fTyp expTyp && (fTyp /= TSelf || expTyp /= recTyp)
                          -> prettyError $  "wrong type of expression assigned to record " <> tid
                                         <> " field " <> field
                                         <> ": expected " <> showType fTyp
                                         <> " but given " <> showType expTyp
                          | otherwise -> return (field, exp)
                return (recTyp, mkRecord tid typedFs)
            _ -> prettyError $ tid <> " isn't a record type"
    Array tid msz mini -> do
        mtyp <- lookupType tid
        (szTyp, sz) <- msz
        (iniTyp, ini) <- mini
        case mtyp of
            Nothing -> prettyError $ "undefined type " <> tid
            Just arrTyp@(TArray elemTyp)
              | szTyp /= TInt
              -> prettyError $  "type mismatch in array size expression: expected int but given "
                             <> showType szTyp
              | isInvalidAssign elemTyp iniTyp
              -> prettyError $ "type mismatch in array init expression: expected "
                             <> showType elemTyp
                             <> "but given " <> showType iniTyp
              | otherwise -> return (arrTyp, mkArray tid sz ini)
            _ -> prettyError $ tid <> " isn't an array type"
    Call fn margs -> do
        mfunTyp <- lookupFun fn
        case mfunTyp of
            Nothing -> prettyError $ "undefined function " <> fn
            Just (TFunc argsTyp retTyp)
              | mustNumArgs <- length argsTyp
              , givenNumArgs <- length margs
              , mustNumArgs /= givenNumArgs
              -> prettyError $  "wrong number of arguments for function call " <> fn
                             <> ": expected " <> showText mustNumArgs
                             <> " but given " <> showText givenNumArgs
              | otherwise -> do
                  typedArgs <- forM (zip margs argsTyp) $ \(marg, typ) -> do
                      (argTyp, arg) <- marg
                      when (isInvalidAssign typ argTyp) $
                          prettyError $  "wrong argument type in function call " <> fn
                                      <> ": expected " <> showType typ
                                      <> " but given " <> showType argTyp
                      return arg
                  return (retTyp, mkCall fn typedArgs)
            _ -> prettyError $ fn <> " isn't a function"
    Assign mlv mrv -> do
        (lvTyp, lv) <- typeCheckLvalF mlv
        (rvTyp, rv) <- mrv
        when (isInvalidAssign lvTyp rvTyp) $
            prettyError $  "type mismatch in assignment expression: expected "
                        <> showType lvTyp
                        <> " but given "
                        <> showType rvTyp
        return (TUnit, mkAssign lv rv)
    If mcond mth mmel -> do
        (condTyp, cond) <- mcond
        (thTyp, th) <- mth
        when (condTyp /= TInt) $
            prettyError $  "type mismatch in if condition: expected int but given "
                        <> showType condTyp
        case mmel of
            Nothing -> return (TUnit, mkIf [cond,th])
            Just mel -> do
                (elTyp, el) <- mel
                let mretTyp = generalType thTyp elTyp
                case mretTyp of
                    Nothing -> prettyError $  "type mismatch in then(" <> showType thTyp
                                           <> ") and else(" <> showType elTyp
                                           <> ") expressions"
                    Just retTyp -> return (retTyp, mkIf [cond,th,el])
    While mcond mbody -> do
        (condTyp, cond) <- mcond
        (_, body) <- mbody
        when (condTyp /= TInt) $
            prettyError $  "type mismatch in while condition: expected int but given "
                        <> showType condTyp
        return (TUnit, mkWhile cond body)
    For var esc mfrom mto mbody -> do
        (fromTyp, from) <- mfrom
        (toTyp, to) <- mto
        mvar <- lookupVar var
        insertVar var TInt
        (_, body) <- mbody
        maybe (deleteVar var) (insertVar var) mvar
        when (fromTyp /= TInt) $
            prettyError $  "type mismatch in for assignment expression: expected int but given "
                        <> showType fromTyp
        when (toTyp /= TInt) $
            prettyError $  "type mismatch in for to expression: expected int but given "
                        <> showType toTyp
        return (TUnit, mkFor var esc from to body)
    Break -> do
        inLoop <- getInLoop
        unless inLoop $ prettyError "illegal break expression outside the loop"
        return (TUnit, mkBreak)
    Let mdecs mexp -> do
        pushEnv
        decs <- typeCheckDecs mdecs
        (expTyp, exp) <- mexp
        popEnv
        return (expTyp, mkLet decs exp)
    Seq mes -> do
        es <- sequenceA mes
        let retTyp = fst $ last es
        return (retTyp, mkSeq $ map snd es)

typeCheckLvalF :: (MonadReader CheckContext m, MonadState CheckState m, MonadError Text m)
               => LValF UntypedDec (m (Type, Expr TypedDec))
               -> m (Type, LValF TypedDec (Expr TypedDec))
typeCheckLvalF lv = case lv of
    Var var -> do
        mtyp <- lookupVar var
        case mtyp of
            Nothing -> prettyError $ "undefined variable " <> var
            Just TFunc{} -> prettyError $ var <> " is a function, not a variable"
            Just typ -> return (typ, Var var)
    Dot mlv field _ -> do
        (lvTyp, lv) <- mlv
        case lvTyp of
            selfTyp@(TRecord tid fs) ->
                case lookup field $ zipWith (\(f,t) o -> (f,(t,o))) fs [0..] of
                    Just (typ, off)
                      | typ == TSelf -> return (selfTyp, Dot lv field off)
                      | otherwise -> return (typ, Dot lv field off)
                    _ -> prettyError $ "record " <> tid <> " have no field " <> field
            _ -> prettyError $ "cannot access field " <> field <> " from non-record type"
    Index mlv mind -> do
        (lvTyp, lv) <- mlv
        (indTyp, ind) <- fmap unFix <$> typeCheckExprF mind
        case lvTyp of
            TArray t
              | indTyp /= TInt
              -> prettyError $  "type mismatch in array indexing: expected int but given "
                             <> showType indTyp
              | otherwise -> return (t, Index lv ind)
            _ -> prettyError "cannot index non-array variable"

typeCheckDecs :: (MonadReader CheckContext m, MonadState CheckState m, MonadError Text m)
              => [UntypedDec (m (Type, Expr TypedDec))]
              -> m [TypedDec (Expr TypedDec)]
typeCheckDecs decs = mapAccumM fPass ([],[]) decs >>= mapM sPass
  where fPass (vars, types) dec = case dec of
            UntypedType tid trv
              | tid `elem` types  -> prettyError $ "redeclaration of type " <> tid
              | TypeId syn <- trv -> insertType' tid (TName syn)
              | TypeRecord fs <- trv -> do
                  let mdup = getDups (map fst fs)
                  case mdup of
                      Just dup ->
                          prettyError $ "redeclaration of record field " <> dup
                      Nothing ->
                          insertType' tid (TRecord tid $ map (\(f,t) -> (f,TName t)) fs)
              | TypeArray t <- trv -> insertType' tid (TArray (TName t))
              where insertType' tid typ = do
                        insertType tid typ
                        return (UntypedType tid trv, (vars, tid:types))
            UntypedVar var mtyp mexp
              | Just vt <- lookup var vars
              -> prettyError $ "redeclaration of " <> vt <> " " <> var
              | otherwise -> return (UntypedVar var mtyp mexp, ((var,"variable"):vars, types))
            UntypedFun fn args mret mexp
              | Just vt <- lookup fn vars
              -> prettyError $ "redeclaration of " <> vt <> " " <> fn
              | otherwise -> do
                  argsTyp <- mapM (lookupTypeErr . snd) args
                  retTyp <- maybe (return TUnit) lookupTypeErr mret
                  let funTyp = TFunc argsTyp retTyp
                  insertFun fn funTyp
                  return (UntypedFun fn args mret mexp, ((fn,"function"):vars, types))
        sPass dec = case dec of
            UntypedType tid _ -> do
                typ <- lookupTypeErr tid
                resTyp <- resolveType typ
                insertType tid resTyp
                return (TypedType tid resTyp)
            UntypedVar var mtyp mexp -> do
                (expTyp, exp) <- mexp
                case mtyp of
                    Nothing
                      | expTyp == TUnit || expTyp == TNil
                      -> prettyError "assign no valued expression to untyped variable"
                      | otherwise -> do
                          insertVar var expTyp
                          return (TypedVar var expTyp False exp)
                    Just typ -> do
                        resTyp <- lookupTypeErr typ
                        when (isInvalidAssign resTyp expTyp) $
                            prettyError $  "type mismatch in variable " <> var
                                        <> " declaration: expected " <> showType resTyp
                                        <> " but given " <> showType expTyp
                        insertVar var resTyp
                        return (TypedVar var resTyp False exp)
            UntypedFun fn args mret mexp -> do
                retTyp <- maybe (return TUnit) lookupTypeErr mret
                typedArgs <- forM args $ \(arg, tid) -> do
                    argTyp <- lookupTypeErr tid
                    return (arg, argTyp, False)
                let funTyp = TFunc (map (\(_,t,_) -> t) typedArgs) retTyp
                insertFun fn funTyp
                pushEnv
                mapM_ (\(a,t,_) -> insertVar a t) typedArgs
                (expTyp, exp) <- mexp
                when (isNothing $ generalType retTyp expTyp) $
                    prettyError $  "type mismatch in body of function " <> fn
                                <> ": expected " <> showType retTyp
                                <> " but given " <> showType expTyp
                popEnv
                return (TypedFun fn typedArgs retTyp exp)
          where resolveType = resolveType' []
                  where resolveType' visited typ = case typ of
                            TArray t -> do
                                resTyp <- resolveType' [] t
                                when (isArray resTyp) $
                                    prettyError "multidimensional arrays aren't supported"
                                return (TArray resTyp)
                            TRecord tid fs -> do
                                typedFs <- forM fs $ \(f,t) ->
                                    if isRecursive tid t
                                       then return (f, TSelf)
                                       else do
                                           resTyp <- resolveType' [] t
                                           return (f, resTyp)
                                return (TRecord tid typedFs)
                            TName syn
                              | syn `elem` visited -> prettyError "cycle in type synonym"
                              | otherwise -> do
                                  synTyp <- lookupTypeErr syn
                                  resTyp <- resolveType' (syn:visited) synTyp
                                  insertType syn resTyp
                                  return resTyp
                            typ -> return typ
                isArray typ = case typ of
                    TArray{} -> True
                    _        -> False
                isRecursive tid typ = case typ of
                    TName syn -> tid == syn
                    _         -> False

isInvalidAssign :: Type -> Type -> Bool
isInvalidAssign t1 t2 = case (t1, t2) of
    (TNil, TNil)      -> True
    (TUnit, TUnit)    -> True
    (TRecord{}, TNil) -> False
    (TNil, TRecord{}) -> False
    (TNil, TSelf)     -> False
    (TSelf, TNil)     -> False
    (t1, t2)          -> t1 /= t2

generalType :: Type -> Type -> Maybe Type
generalType t1 t2 = case (t1, t2) of
    (TNil, TRecord{})    -> Just t2
    (TRecord{}, TNil)    -> Just t1
    (t1, t2) | t1 == t2  -> Just t1
             | otherwise -> Nothing

getDups :: Eq a => [a] -> Maybe a
getDups = getDups' []
 where getDups' xs (y:ys)
         | y `elem` xs = Just y
         | otherwise = getDups' (y:xs) ys
       getDups' _ _ = Nothing
