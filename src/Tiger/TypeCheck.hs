{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiger.TypeCheck (typeCheck) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Fix
import Data.HashMap.Strict (HashMap)
import Data.List (find)
import Data.Text hiding ( length
                        , head
                        , tail
                        , last
                        , map
                        , zip
                        , zipWith
                        , null
                        , filter
                        , find
                        , elem
                        )
import Data.Maybe (listToMaybe, mapMaybe, isNothing)
import Data.Functor.Compose

import Tiger.Utils
import Tiger.Expr

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap

data Env = Env
    { venv :: HashMap Text Type
    , tenv :: HashMap Text Type
    }

data CheckContext = CheckContext
    { inLoop     :: Bool
    , sourceCode :: Text
    }

data CheckState = CheckState
    { envs   :: [Env]
    , curPos :: SrcSpan
    }

withNewEnv :: MonadState CheckState m => m a -> m a
withNewEnv ma = do
    modify (\st -> st{envs = Env HashMap.empty HashMap.empty:envs st})
    res <- ma
    modify (\st -> st{envs = tail $ envs st})
    return res

getCurPos :: MonadState CheckState m => m SrcSpan
getCurPos = gets curPos

setCurPos :: MonadState CheckState m => SrcSpan -> m ()
setCurPos pos = modify (\st -> st {curPos = pos})

withinLoop :: MonadReader CheckContext m => m a -> m a
withinLoop = local (\st -> st {inLoop = True})

getInLoop :: MonadReader CheckContext m => m Bool
getInLoop = reader inLoop

getSourceCode :: MonadReader CheckContext m => m Text
getSourceCode = reader sourceCode

lookupVar :: MonadState CheckState m => Text -> m (Maybe Type)
lookupVar var = gets (listToMaybe . mapMaybe (HashMap.lookup var . venv) . envs)

lookupFun :: MonadState CheckState m => Text -> m (Maybe Type)
lookupFun = lookupVar

lookupType :: MonadState CheckState m => Text -> m (Maybe Type)
lookupType typ = gets (listToMaybe . mapMaybe (HashMap.lookup typ . tenv) . envs)

lookupTypeErr :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
              => Text
              -> m Type
lookupTypeErr tid =
    lookupType tid >>= maybe (prettyError $ "undefined type " <> tid) return

insertVar :: MonadState CheckState m => Text -> Type -> m ()
insertVar var typ = modify $ \st ->
    let (e:es) = envs st
    in st{envs = e{venv = HashMap.insert var typ $ venv e}:es}

insertFun :: MonadState CheckState m => Text -> Type -> m ()
insertFun = insertVar

insertType :: MonadState CheckState m => Text -> Type -> m ()
insertType tid typ = modify $ \st ->
    let (e:es) = envs st
    in st{envs = e{tenv = HashMap.insert tid typ $ tenv e}:es}

deleteVar :: MonadState CheckState m => Text -> m ()
deleteVar var = modify $ \st ->
    let (e:es) = envs st
    in st{envs = e{venv = HashMap.delete var $ venv e}:es}

prettyError :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
            => Text
            -> m a
prettyError err = do
    SourcePos fpath ln cl <- spanBegin <$> getCurPos
    src <- getSourceCode
    let strLn = showText ln
        margin = Text.replicate (Text.length strLn + 2) " "
        errMsg =  pack fpath <> ":" <> strLn <> ":" <> showText cl
               <> ": error:\n" <> err <> "\n"
        line = Text.takeWhile (/='\n')
             . (!!(ln-1))
             . iterate (Text.tail . Text.dropWhile (/= '\n')) $ src
        prettyLine =  margin <> "|\n " <> strLn <> " | " <> line <> "\n"
                   <> margin <> "|\n"
    throwError $ errMsg <> prettyLine

typeCheck :: Text -> PosExpr UntypedDec -> Either Text (Expr TypedDec)
typeCheck src expr =
    runExcept (evalStateT (runReaderT (run expr) initCtx) initSt)
  where run = fmap snd . adi (typeCheckExprF . annotated . getCompose) setContext
        initSt = CheckState { envs   = initEnvs
                            , curPos = SrcSpan iniPos iniPos
                            }
        initCtx = CheckContext { inLoop     = False
                               , sourceCode = src
                               }
        iniPos = SourcePos "" 1 1
        initEnvs =
            [ Env { tenv = HashMap.fromList
                               [ ("int", TInt)
                               , ("string", TString)
                               ]
                  , venv = HashMap.fromList
                               [ ("print", TFunc [TString] TUnit)
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

setContext :: (MonadReader CheckContext m, MonadState CheckState m)
           => (PosExpr UntypedDec -> m (Type, Expr TypedDec))
           -> PosExpr UntypedDec
           -> m (Type, Expr TypedDec)
setContext f = \case
    expr@(AnnE ann For{})   -> loop ann expr
    expr@(AnnE ann While{}) -> loop ann expr
    expr -> do
        setCurPos (annotation $ getCompose $ unFix expr)
        f expr
  where loop ann expr = withinLoop $ do
                setCurPos ann
                f expr

typeCheckExprF :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
               => ExprF UntypedDec (m (Type, Expr TypedDec))
               -> m (Type, Expr TypedDec)
typeCheckExprF = \case
    IntLit i -> return (TInt, mkIntLit i)
    StrLit s -> return (TString, mkStrLit s)
    Nil      -> return (TNil, mkNil)
    LVal lv  -> fmap mkLVal <$> typeCheckLvalF lv
    Neg mexpr -> do
        (exprTyp, expr) <- mexpr
        when (exprTyp /= TInt) $
            prettyError $  "type mismatch in neg expression: expected int but given "
                        <> showType exprTyp
        return (TInt, mkNeg expr)
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
      where eqne
              :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
              => Type
              -> Type
              -> m ()
            eqne typ1 typ2 = when (isInvalidAssign typ1 typ2) $
                prettyError $  "type mismatch in " <> showBinop op
                            <> " operation: trying to compare " <> showType typ1
                            <> " with " <> showType typ2

            opError
              :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
              => Type
              -> m ()
            opError typ = prettyError $  "type mismatch in " <> showBinop op
                                      <> " operation: expected int but given "
                                      <> showType typ
    Record tid mfs -> do
        mtyp <- lookupType tid
        case mtyp of
            Nothing -> prettyError $ "undefined type " <> tid
            Just recTyp@(TRecord recId recFs) -> do
                let mdup = findDupRecordFields mfs
                maybe (return ())
                      (prettyError . ("repeated assignment to record field " <>))
                      mdup
                when (length mfs /= length recFs) $
                    prettyError "not all fields of record type are initialized"
                typedFs <- forM mfs $ \(RecordField field mexpr) -> do
                    (exprTyp, expr) <- mexpr
                    case find ((== field) . rfName) recFs of
                        Nothing -> prettyError $  "record type " <> recId
                                               <> " have no field " <> field
                        Just (RecordField _ fTyp)
                          |  isInvalidAssign fTyp exprTyp
                          && (fTyp /= TSelf || exprTyp /= recTyp)
                          -> prettyError
                            $  "wrong type of expression assigned to record " <> tid
                            <> " field " <> field
                            <> ": expected " <> showType fTyp
                            <> " but given " <> showType exprTyp
                          | otherwise -> return (RecordField field expr)
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
              -> prettyError
                $  "type mismatch in array size expression: expected int but given "
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
            prettyError
                $  "type mismatch in for assignment expression: expected int but given "
                <> showType fromTyp
        when (toTyp /= TInt) $
            prettyError $  "type mismatch in for to expression: expected int but given "
                        <> showType toTyp
        return (TUnit, mkFor var esc from to body)
    Break -> do
        isInLoop <- getInLoop
        unless isInLoop $ prettyError "illegal break expression outside the loop"
        return (TUnit, mkBreak)
    Let mdecs mexpr -> do
        (exprTyp, expr, decs) <- withNewEnv $ do
            decs <- typeCheckDecs mdecs
            (exprTyp, expr) <- mexpr
            return (exprTyp, expr, decs)
        return (exprTyp, mkLet decs expr)
    Seq mes -> do
        es <- sequenceA mes
        let retTyp = fst $ last es
        return (retTyp, mkSeq $ map snd es)

typeCheckLvalF :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
               => LValF UntypedDec (m (Type, Expr TypedDec))
               -> m (Type, LValF TypedDec (Expr TypedDec))
typeCheckLvalF = \case
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
                case lookup field $ recordFieldIndices fs of
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
  where recordFieldIndices :: [RecordField a] -> [(Text, (a, Int))]
        recordFieldIndices = zipWith (\ind rf -> (rfName rf, (rfValue rf, ind))) [0..]

typeCheckDecs :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
              => [UntypedDec (m (Type, Expr TypedDec))]
              -> m [TypedDec (Expr TypedDec)]
typeCheckDecs decs =
    mapAccumM typeCheckFirstPass ([],[]) decs >>= mapM typeCheckSecondPass

data VarInfo = Variable Text
             | Function Text

varInfoName :: VarInfo -> Text
varInfoName (Variable v) = v
varInfoName (Function f) = f

showVarInfo :: VarInfo -> Text
showVarInfo (Variable v) = "variable " <> v
showVarInfo (Function f) = "function " <> f

typeCheckFirstPass
    :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
    => ([VarInfo], [Text])
    -> UntypedDec (m (Type, Expr TypedDec))
    -> m (UntypedDec (m (Type, Expr TypedDec)), ([VarInfo], [Text]))
typeCheckFirstPass (vars, types) = \case
    typDec@(UntypedTypeDec (UntypedType tid trv))
      | tid `elem` types  -> prettyError $ "redeclaration of type " <> tid
      | TypeId syn <- trv -> insertTypeAndReturn tid (TName syn)
      | TypeRecord fs <- trv -> do
          let mdup = findDupRecordFields fs
          case mdup of
              Just dup ->
                  prettyError $ "redeclaration of record field " <> dup
              Nothing ->
                  insertTypeAndReturn tid (TRecord tid $ map (fmap TName) fs)
      | TypeArray t <- trv -> insertTypeAndReturn tid (TArray (TName t))
      where insertTypeAndReturn tName typ = do
                insertType tName typ
                return (typDec, (vars, tName:types))
    varDec@(UntypedVarDec var)
      | Just varInfo <- find ((== uVarName var) . varInfoName) vars
      -> prettyError $ "redeclaration of " <> showVarInfo varInfo
      | otherwise
      -> return (varDec, (Variable (uVarName var):vars, types))
    funDec@(UntypedFunDec (UntypedFun fn args mret _))
      | Just varInfo <- find ((== fn) . varInfoName) vars
      -> prettyError $ "redeclaration of " <> showVarInfo varInfo
      | otherwise -> do
          argsTyp <- mapM (lookupTypeErr . uFunArgType) args
          retTyp <- maybe (return TUnit) lookupTypeErr mret
          let funTyp = TFunc argsTyp retTyp
          insertFun fn funTyp
          return (funDec, (Function fn:vars, types))

typeCheckSecondPass
    :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
    => UntypedDec (m (Type, Expr TypedDec))
    -> m (TypedDec (Expr TypedDec))
typeCheckSecondPass = \case
    UntypedTypeDec (UntypedType tid _) -> do
        typ <- lookupTypeErr tid
        resTyp <- resolveType typ
        insertType tid resTyp
        return (mkTypedType tid resTyp)
    UntypedVarDec (UntypedVar var mtyp mexpr) -> do
        (exprTyp, expr) <- mexpr
        case mtyp of
            Nothing
              | exprTyp == TUnit || exprTyp == TNil
              -> prettyError "assign no valued expression to untyped variable"
              | otherwise -> do
                  insertVar var exprTyp
                  return (TypedVarDec (TypedVar var exprTyp Remaining expr))
            Just typ -> do
                resTyp <- lookupTypeErr typ
                when (isInvalidAssign resTyp exprTyp) $
                    prettyError $  "type mismatch in variable " <> var
                                <> " declaration: expected " <> showType resTyp
                                <> " but given " <> showType exprTyp
                insertVar var resTyp
                return (mkTypedVar var resTyp Remaining expr)
    UntypedFunDec (UntypedFun fn args mret mexpr) -> do
        retTyp <- maybe (return TUnit) lookupTypeErr mret
        typedArgs <- forM args $ \(UntypedFunArg arg tid) -> do
            argTyp <- lookupTypeErr tid
            return (TypedFunArg arg argTyp Remaining)
        let funTyp = TFunc (map tFunArgType typedArgs) retTyp
        insertFun fn funTyp
        expr <- withNewEnv $ do
            forM_ typedArgs $ \tfa ->
                insertVar (tFunArgName tfa) (tFunArgType tfa)
            (exprTyp, expr) <- mexpr
            when (isNothing $ generalType retTyp exprTyp) $
                prettyError $  "type mismatch in body of function " <> fn
                            <> ": expected " <> showType retTyp
                            <> " but given " <> showType exprTyp
            return expr
        return (mkTypedFun fn typedArgs retTyp expr)

resolveType :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
            => Type
            -> m Type
resolveType = resolveType' []
  where resolveType'
            :: (MonadError Text m, MonadReader CheckContext m, MonadState CheckState m)
            => [Text]
            -> Type
            -> m Type
        resolveType' visited = \case
            TArray t -> do
                resTyp <- resolveType' [] t
                when (isArray resTyp) $
                    prettyError "multidimensional arrays aren't supported"
                return (TArray resTyp)
            TRecord tid fs -> do
                typedFs <- forM fs $ \(RecordField f t) ->
                    if isRecursive tid t
                       then return (RecordField f TSelf)
                       else do
                           resTyp <- resolveType' [] t
                           return (RecordField f resTyp)
                return (TRecord tid typedFs)
            TName syn
              | syn `elem` visited -> prettyError "cycle in type synonym"
              | otherwise -> do
                  synTyp <- lookupTypeErr syn
                  resTyp <- resolveType' (syn:visited) synTyp
                  insertType syn resTyp
                  return resTyp
            typ -> return typ
        isArray = \case
            TArray{} -> True
            _        -> False
        isRecursive tid = \case
            TName syn -> tid == syn
            _         -> False

isInvalidAssign :: Type -> Type -> Bool
isInvalidAssign typ1 typ2 = case (typ1, typ2) of
    (TNil, TNil)      -> True
    (TUnit, TUnit)    -> True
    (TRecord{}, TNil) -> False
    (TNil, TRecord{}) -> False
    (TNil, TSelf)     -> False
    (TSelf, TNil)     -> False
    (t1, t2)          -> t1 /= t2

generalType :: Type -> Type -> Maybe Type
generalType typ1 typ2 = case (typ1, typ2) of
    (TNil, TRecord{})    -> Just typ2
    (TRecord{}, TNil)    -> Just typ1
    (t1, t2) | t1 == t2  -> Just t1
             | otherwise -> Nothing

findDupRecordFields :: [RecordField a] -> Maybe Text
findDupRecordFields = getDups []
 where getDups :: [Text] -> [RecordField a] -> Maybe Text
       getDups xs (RecordField yName _:ys)
         | yName `elem` xs = Just yName
         | otherwise = getDups (yName:xs) ys
       getDups _ _ = Nothing
