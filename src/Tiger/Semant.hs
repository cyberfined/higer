{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

module Tiger.Semant
    ( Type(..)
    , Unique(..)
    , SemantException(..)
    , PosedSemantException(..)
    , posedExceptionToText
    , exceptionToText
    , semantAnalyze
    ) where

import           Control.Exception          (Exception, throwIO, try)
import           Control.Monad              (foldM, forM, forM_, unless, when)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (MonadReader, ReaderT, asks, runReaderT)
import           Data.Functor               (($>))
import           Data.HashMap.Strict        (HashMap)
import           Data.IORef                 (IORef, modifyIORef', newIORef, readIORef,
                                             writeIORef)
import           Data.List                  (delete)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           Prelude                    hiding (exp, span)

import           Tiger.Expr
import           Tiger.Frame                hiding (Access (..), accessToIR, allocLocal)
import           Tiger.IR                   (ExternalFun (..), IR, IRData (..),
                                             IRFunction (..), LabeledString (..), Operand,
                                             Stmt)
import           Tiger.Temp

import qualified Data.HashMap.Strict        as HashMap
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

import qualified Tiger.Frame                as Frame
import qualified Tiger.IR                   as IR

data Type
    = TInt
    | TString
    | TRecord !Text ![(Text, Type)] !Unique
    | TArray !Type !Unique
    | TNil
    | TUnit
    | TName !TypeRef
    deriving Eq

isTypesMatch :: Type -> Type -> Bool
isTypesMatch TInt TInt                         = True
isTypesMatch TString TString                   = True
isTypesMatch TRecord{} TNil                    = True
isTypesMatch TNil TRecord{}                    = True
isTypesMatch (TRecord _ _ u1) (TRecord _ _ u2) = u1 == u2
isTypesMatch (TArray _ u1) (TArray _ u2)       = u1 == u2
isTypesMatch TNil TNil                         = True
isTypesMatch TUnit TUnit                       = True
isTypesMatch (TName r1) (TName r2)             = r1 == r2
isTypesMatch _ _                               = False

instance Show Type where
    show = Text.unpack . typeToText

typeToText :: Type -> Text
typeToText = \case
    TInt            -> "int"
    TString         -> "string"
    TRecord rec _ _ -> rec
    TArray typ _    -> typeToText typ <> "[]"
    TNil            -> "nil"
    TUnit           -> "unit"
    TName{}         -> "name"

newtype Unique = Unique Int deriving Eq

newtype TypeRef = TypeRef (IORef (Maybe Type)) deriving Eq

data Level f = Level !Unique !(Maybe (Level f)) !f

instance Frame f => Eq (Level f) where
    Level u1 _ _ == Level u2 _ _ = u1 == u2

data Access f = Access !(Level f) !Frame.Access

newtype SemantM f a = SemantM { runSemantM :: ReaderT (Context f) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (Context f), MonadIO)

data Context f = Context
    { ctxUnique             :: !(IORef Unique)
    , ctxNextTemp           :: !(IORef Int)
    , ctxNextLabel          :: !(IORef Int)
    , ctxEnvs               :: !(IORef [(HashMap Text (EnvEntry f), HashMap Text Type)])
    , ctxSpan               :: !(IORef Span)
    , ctxLoopEndLabel       :: !(IORef (Maybe Label))
    , ctxCurrentLevel       :: !(IORef (Level f))
    , ctxStrings            :: !(IORef (HashMap Text Label))
    , ctxIRFunctions        :: !(IORef [IRFunction f])
    , ctxCurrentIRFunctions :: !(IORef [IRFunction f])
    }

data EnvEntry f
    = VarEntry !(VarInfo f)
    | FunEntry !FunInfo

data VarInfo f = VarInfo
    { varInfoType   :: !Type
    , varInfoAccess :: !(Access f)
    }

data FunInfo = FunInfo
    { funInfoArgs  :: ![Type]
    , funInfoRet   :: !Type
    , funInfoLabel :: !Label
    }

data SemantException
    = UndefinedVariable !Text
    | UndefinedType !Text
    | RecordHasNoField !Text !Text
    | NotRecord !Text
    | NotArray !Text
    | TypeMismatch !Type !Type
    | DuplicatedRecordField !Text
    | CycleTypeDec !Text
    | EmptyName
    | NotRecordType !Type
    | UnitializedRecordField !Text !Text
    | NotArrayType !Type
    | BreakOutsideLoop
    | UnitAssignment
    | UndefinedFunction !Text
    | ArgumentsNumberMismatch !Int !Int
    | UnitComparison
    | NilAssignmentWithoutType
    deriving Show

exceptionToText :: SemantException -> Text
exceptionToText = \case
    UndefinedVariable var            -> "undefined variable " <> var
    UndefinedType typ                -> "undefined type " <> typ
    RecordHasNoField rec field       -> "record " <> rec <> " has no field " <> field
    NotRecord lval                   -> lval <> " is not a record"
    NotArray lval                    -> lval <> " is not an array"
    TypeMismatch exp act             -> "type mismatch: expecting " <> typeToText exp
                                     <> ", actual " <> typeToText act
    DuplicatedRecordField field      -> "duplicated record field " <> field
    CycleTypeDec typ                 -> "type " <> typ <> " forms cycle"
    EmptyName                        -> "name reference is empty"
    NotRecordType typ                -> typeToText typ <> " is not a record type"
    UnitializedRecordField rec field -> "field " <> field <> " of " <> rec
                                     <> " is unitialized"
    NotArrayType typ                 -> typeToText typ <> " is not an array type"
    BreakOutsideLoop                 -> "break is outside the loop"
    UnitAssignment                   -> "can't assign unit type expression"
    UndefinedFunction fun            -> "undefined function " <> fun
    ArgumentsNumberMismatch exp act  -> "wrong number of arguments: expecting "
                                     <> (Text.pack $ show exp) <> ", actual "
                                     <> (Text.pack $ show act)
    UnitComparison                   -> "unit type expressions can't be compared"
    NilAssignmentWithoutType         -> "can't assign nil without type constraint"

instance Exception SemantException

data PosedSemantException = PosedSemantException !FilePath !SemantException !Span

posedExceptionToText :: PosedSemantException -> Text
posedExceptionToText (PosedSemantException file err span)
  =  Text.pack file <> Text.pack (':':show l1) <> Text.pack (':':show c1)
  <> Text.pack ('-':show l2) <> Text.pack (':':show c2)
  <> ": " <> exceptionToText err
  where Span (Position l1 c1) (Position l2 c2) = span

instance Frame f => MonadTemp (SemantM f) where
    newLabel = do
        nextLabelRef <- asks ctxNextLabel
        l <- liftIO (readIORef nextLabelRef)
        liftIO $ modifyIORef' nextLabelRef (+1)
        pure (LabelInt l)
    newTemp = do
        nextTempRef <- asks ctxNextTemp
        t <- liftIO (readIORef nextTempRef)
        liftIO $ modifyIORef' nextTempRef (+1)
        pure (Temp t)

class (Frame f, MonadTemp (m f)) => MonadSemant m f where
    newUnique       :: m f Unique
    newTypeRef      :: m f TypeRef
    readTypeRef     :: TypeRef -> m f (Maybe Type)
    writeTypeRef    :: TypeRef -> Type -> m f ()
    withNewEnv      :: m f a -> m f a
    initMainFrame   :: m f ()
    newFuncLabel    :: Text -> m f Label
    emitLabel       :: m f Label
    withNewFrame    :: Label -> [Escaping] -> ([Access f] -> m f a) -> m f a
    allocLocal      :: Escaping -> m f (Access f)
    insertVarType   :: Text -> (EnvEntry f) -> m f ()
    insertType      :: Text -> Type -> m f ()
    getVarInfo      :: Text -> m f (VarInfo f)
    getFunInfo      :: Text -> m f FunInfo
    getType         :: Text -> m f Type
    withLoop        :: Label -> m f a -> m f a
    getLoopEndLabel :: m f (Maybe Label)
    setSpan         :: Span -> m f ()
    getStringLabel  :: Text -> m f Label
    emitIR          :: IR Stmt -> m f ()
    accessToIR      :: Access f -> m f Temp
    getCurrentFrame :: m f f
    throwError      :: SemantException -> m f a

instance Frame f => MonadSemant SemantM f where
    newUnique = do
        uniqueRef <- asks ctxUnique
        uniq@(Unique cur) <- liftIO $ readIORef uniqueRef
        liftIO $ writeIORef uniqueRef $ Unique (cur + 1)
        pure uniq
    newTypeRef = TypeRef <$> liftIO (newIORef Nothing)
    readTypeRef (TypeRef ref) = liftIO (readIORef ref)
    writeTypeRef (TypeRef ref) typ = liftIO (writeIORef ref $ Just typ)
    withNewEnv f = do
        envsRef <- asks ctxEnvs
        newEnvs <- liftIO $ readIORef envsRef >>= \case
            x:_ -> pure x
            _   -> pure (HashMap.empty, HashMap.empty)
        liftIO $ modifyIORef' envsRef (newEnvs:)
        res <- f
        liftIO $ modifyIORef' envsRef tail
        pure res
    newFuncLabel funName = do
        nextLabelRef <- asks ctxNextLabel
        l <- liftIO (readIORef nextLabelRef)
        liftIO $ modifyIORef' nextLabelRef (+1)
        let labelName = LazyText.toStrict $
                        Builder.toLazyText $
                        Builder.fromText funName <> "_" <> Builder.decimal l
        pure (LabelText labelName)
    emitLabel = do
        IRFunction{..} <- head <$> (asks ctxCurrentIRFunctions >>= liftIO . readIORef)
        case irFuncBody of
            (IR.Label l:_) -> pure l
            _              -> newLabel >>= \l -> emitIR (IR.Label l) $> l
    withNewFrame label args f = do
        frame <- newFrame @f label (Escaping : args)
        currentLevelRef <- asks ctxCurrentLevel
        currentLevel <- liftIO $ readIORef currentLevelRef
        levelUniq <- newUnique
        let level = Level levelUniq (Just currentLevel) frame
        currentIRFunctionsRef <- asks ctxCurrentIRFunctions

        liftIO $ do
            writeIORef currentLevelRef level
            modifyIORef' currentIRFunctionsRef ((IRFunction [] frame):)

        let (entryStmts, exitStmts) = Frame.procEntryExit frame
        mapM_ emitIR entryStmts
        res <- f (map (Access level) $ tail $ frameArgs frame)
        mapM_ emitIR exitStmts

        irFunctionsRef <- asks ctxIRFunctions
        liftIO $ do
            writeIORef currentLevelRef currentLevel
            (func@IRFunction{..}:fs) <- readIORef currentIRFunctionsRef
            modifyIORef' irFunctionsRef (func { irFuncBody = reverse irFuncBody }:)
            writeIORef currentIRFunctionsRef fs

        pure res
    initMainFrame = do
        mainFrame <- newFrame @f (LabelText "main") []
        mainUniq <- newUnique
        let mainLevel = Level mainUniq Nothing mainFrame
        currentLevelRef <- asks ctxCurrentLevel
        irFunctionsRef <- asks ctxCurrentIRFunctions
        liftIO $ do
            writeIORef currentLevelRef mainLevel
            writeIORef irFunctionsRef [IRFunction [] mainFrame]
    allocLocal esc = do
        level@(Level _ _ frame) <- asks ctxCurrentLevel >>= liftIO . readIORef
        access <- Frame.allocLocal frame esc
        pure (Access level access)
    insertVarType name entry = do
        envsRef <- asks ctxEnvs
        liftIO $ modifyIORef' envsRef $ \case
            ((varEnv,typeEnv):xs) -> (HashMap.insert name entry varEnv, typeEnv):xs
            _                     -> [(HashMap.singleton name entry, HashMap.empty)]
    insertType name entry = do
        envsRef <- asks ctxEnvs
        liftIO $ modifyIORef' envsRef $ \case
            ((varEnv, typeEnv):xs) -> (varEnv, HashMap.insert name entry typeEnv):xs
            _                      -> [(HashMap.empty, HashMap.singleton name entry)]
    getVarInfo name = asks ctxEnvs >>= liftIO . readIORef >>= \case
        (varEnv, _):_ -> case HashMap.lookup name varEnv of
            Just (VarEntry info) -> pure info
            _                    -> liftIO $ throwIO $ UndefinedVariable name
        _ -> liftIO $ throwIO $ UndefinedVariable name
    getFunInfo name = asks ctxEnvs >>= liftIO . readIORef >>= \case
        (varEnv, _):_ -> case HashMap.lookup name varEnv of
            Just (FunEntry info) -> pure info
            _                    -> liftIO $ throwIO $ UndefinedFunction name
        _ -> liftIO $ throwIO $ UndefinedFunction name
    getType name = do
        asks ctxEnvs >>= liftIO . readIORef >>= \case
            (_, typeEnv):_ -> case HashMap.lookup name typeEnv of
                Just entry -> pure entry
                Nothing    -> liftIO $ throwIO $ UndefinedType name
            _ -> liftIO $ throwIO $ UndefinedType name
    withLoop endLab f = do
        loopEndLabelRef <- asks ctxLoopEndLabel
        initValue <- liftIO $ readIORef loopEndLabelRef
        liftIO $ writeIORef loopEndLabelRef (Just endLab)
        res <- f
        liftIO $ writeIORef loopEndLabelRef initValue
        pure res
    getLoopEndLabel = asks ctxLoopEndLabel >>= liftIO . readIORef
    setSpan s = asks ctxSpan >>= \spanRef -> liftIO $ writeIORef spanRef s
    getStringLabel str = do
        stringsRef <- asks ctxStrings
        HashMap.lookup str <$> liftIO (readIORef stringsRef) >>= \case
            Just label -> pure label
            _ -> do
                label <- newLabel
                liftIO $ modifyIORef' stringsRef (HashMap.insert str label)
                pure label
    emitIR ir = do
        irFunctionsRef <- asks ctxCurrentIRFunctions
        let insertIr (f@IRFunction{..} : fs) = f { irFuncBody = ir : irFuncBody } : fs
            insertIr _                       = error "ctxCurrentIRFunctions is empty"
        liftIO $ modifyIORef' irFunctionsRef insertIr
    accessToIR (Access level@(Level _ _ frame) access) = do
        curLevel <- asks ctxCurrentLevel >>= liftIO . readIORef
        fp <- followStaticLinks (IR.Temp FP) curLevel
        (stmts, op) <- Frame.accessToIR frame access fp
        mapM_ emitIR stmts
        pure op
      where followStaticLinks fp innerLevel@(Level _ mParentLevel innerFrame)
              | innerLevel == level = pure fp
              | otherwise = do
                  stLink <- getStaticLink innerFrame
                  case mParentLevel of
                      Nothing          -> error "level does not have a parent"
                      Just parentLevel -> followStaticLinks stLink parentLevel
    getCurrentFrame =  irFuncFrame . head
                   <$> (asks ctxCurrentIRFunctions >>= liftIO . readIORef)
    throwError = liftIO . throwIO

getStaticLink :: (Frame f, MonadSemant m f) => f -> m f (IR Operand)
getStaticLink frame = do
    (stmts, op) <- Frame.accessToIR frame stLinkAccess (IR.Temp FP)
    mapM_ emitIR stmts
    stLink <- newTemp
    emitIR (IR.Load stLink (IR.Temp op))
    pure (IR.Temp stLink)
  where stLinkAccess = head $ Frame.frameArgs frame

semantAnalyze :: forall f. Frame f
              => FilePath
              -> Expr
              -> IO (Either PosedSemantException (IRData f))
semantAnalyze file expr = do
    uniqueRef <- newIORef (Unique 0)
    nextTempRef <- newIORef 0
    nextLabelRef <- newIORef 0
    envsRef <- newIORef [(libFunctions, initTypes)]
    spanRef <- newIORef (Span (Position 0 0) (Position 0 0))
    loopEndLabelRef <- newIORef Nothing
    currentLevelRef <- newIORef undefined
    stringsRef <- newIORef HashMap.empty
    irFunctionsRef <- newIORef []
    currentIRFunctionsRef <- newIORef []
    let ctx = Context { ctxUnique             = uniqueRef
                      , ctxNextTemp           = nextTempRef
                      , ctxNextLabel          = nextLabelRef
                      , ctxEnvs               = envsRef
                      , ctxSpan               = spanRef
                      , ctxLoopEndLabel       = loopEndLabelRef
                      , ctxCurrentLevel       = currentLevelRef
                      , ctxStrings            = stringsRef
                      , ctxIRFunctions        = irFunctionsRef
                      , ctxCurrentIRFunctions = currentIRFunctionsRef
                      }
    let transMain = initMainFrame >> transExpr expr
        eval :: IO (Either SemantException (Type, IR Operand))
        eval = try (runReaderT (runSemantM transMain) ctx)
    eval >>= \case
        Left err -> do
            span <- readIORef spanRef
            pure (Left $ PosedSemantException file err span)
        Right{}  -> do
            let toLabeledString k v a = LabeledString k v : a
            strings <- HashMap.foldrWithKey' toLabeledString [] <$> readIORef stringsRef
            readIORef currentIRFunctionsRef >>= \case
                (f@IRFunction{..}:_) ->
                    modifyIORef' irFunctionsRef (f { irFuncBody = reverse irFuncBody } :)
                _     -> pure ()
            irFunctions <- readIORef irFunctionsRef
            pure $ Right $ IRData { resStrings   = strings
                                  , resFunctions = irFunctions
                                  }
  where initTypes = HashMap.fromList [("int", TInt), ("string", TString)]

libFunctions :: Frame f => HashMap Text (EnvEntry f)
libFunctions = HashMap.fromList $
    map (\(n, c) -> (n, FunEntry $ c (LabelText n)))
        [ ("print", FunInfo [TString] TUnit)
        , ("flush", FunInfo [] TUnit)
        , ("getchar", FunInfo [] TString)
        , ("ord", FunInfo [TString] TInt)
        , ("chr", FunInfo [TInt] TString)
        , ("size", FunInfo [TString] TInt)
        , ("substring", FunInfo [TString, TInt, TInt] TString)
        , ("concat", FunInfo [TString, TString] TString)
        , ("not", FunInfo [TInt] TInt)
        , ("exit", FunInfo [TInt] TUnit)
        ]

transExpr :: forall m f. MonadSemant m f => Expr -> m f (Type, IR Operand)
transExpr expr = do
    setSpan (exprSpan expr)
    case expr of
        LVal lval-> do
            (lvalType, loc, op) <- transLVal lval
            (lvalType,) <$> getVarValue (IR.Temp op) loc
        Nil _ -> pure (TNil, IR.Const 0)
        IntLit i _ -> pure (TInt, IR.Const i)
        StrLit str _ -> do
            lab <- getStringLabel str
            pure (TString, IR.Name lab)
        Neg e _ -> do
            op <- checkInt e
            res <- newTemp
            emitIR (IR.Binop res IR.Sub (IR.Const 0) op)
            pure (TInt, IR.Temp res)
        Binop e1 op e2 _ -> (TInt, ) <$> transBinop op e1 e2
        Record typeName fields _ -> getActualType typeName >>= \case
            typ@(TRecord _ typeFields _) -> do
                recPtr <- newTemp
                fieldPtr <- newTemp
                let sizeOp = IR.Const $ length typeFields
                let initOp = IR.Const 0
                emitIR $ Frame.externalCall (Proxy @f) (Just recPtr) CreateArray
                                                                     [sizeOp, initOp]

                let checkFields (Field{..}:fs) notSeenFields seenFields
                      | fieldName `elem` seenFields
                      = throwErrorPos (DuplicatedRecordField fieldName) fieldSpan
                      | Just (fieldType, fieldIdx) <- lookupIndex fieldName typeFields
                      = do
                          (fieldValueType, fieldOp) <- transExpr fieldValue
                          actFieldType <- actualType fieldType
                          unless (isTypesMatch actFieldType fieldValueType) $
                              throwErrorPos (TypeMismatch actFieldType fieldValueType)
                                            fieldSpan

                          let offset = IR.Const $ fieldIdx * wordSize (Proxy @f)
                          emitIR (IR.Binop fieldPtr IR.Add (IR.Temp recPtr) offset)
                          fieldTemp <- operandToTemp fieldOp
                          emitIR (IR.Store (IR.Temp fieldPtr) fieldTemp)
                          checkFields fs (delete fieldName notSeenFields)
                              (fieldName : seenFields)
                      | otherwise
                      = throwError (RecordHasNoField typeName fieldName)
                    checkFields _ (f:_) _ = throwError (UnitializedRecordField typeName f)
                    checkFields _ _ _     = pure ()

                checkFields fields (map fst typeFields) []
                pure (typ, IR.Temp recPtr)
            typ -> throwError (NotRecordType typ)
        Array typeName sizeExpr initExpr _ -> getActualType typeName >>= \case
            typ@(TArray elemType _) -> do
                actElemType <- actualType elemType
                sizeOp <- checkInt sizeExpr
                (initExprType, initOp) <- transExpr initExpr
                unless (isTypesMatch actElemType initExprType) $
                    throwError (TypeMismatch actElemType initExprType)
                arrPtr <- newTemp
                emitIR $ Frame.externalCall (Proxy @f) (Just arrPtr) CreateArray
                                                                     [sizeOp, initOp]
                pure (typ, IR.Temp arrPtr)
            typ -> throwError (NotArrayType typ)
        Assign lval rval _ -> do
            (lvalType, loc, lvalOp) <- transLVal lval
            (rvalType, rvalOp) <- transExpr rval
            unless (isTypesMatch lvalType rvalType) $
                throwErrorPos (TypeMismatch lvalType rvalType) (exprSpan rval)
            assign lvalOp rvalOp loc
            pure (TUnit, IR.Const 0)
        If cond th mel _ -> do
            (tLab, fLab) <- (,) <$> newLabel <*> newLabel
            transExprCond tLab fLab cond

            emitIR (IR.Label tLab)
            case mel of
                Just el -> do
                    resLab <- newLabel
                    (thType', thOp) <- transExpr th
                    thType <- actualType thType'
                    mRes <- if thType == TUnit
                               then pure Nothing
                               else do
                                   t <- newTemp
                                   emitIR (IR.Assign t thOp)
                                   pure $ Just t
                    emitIR (IR.Jump resLab)
                    emitIR (IR.Label fLab)
                    (elType', elOp) <- transExpr el
                    elType <- actualType elType'
                    maybe (pure ()) (\t -> emitIR (IR.Assign t elOp)) mRes
                    emitIR (IR.Label resLab)
                    unless (isTypesMatch thType elType) $
                        throwError (TypeMismatch thType elType)
                    let resOp = maybe (IR.Const 0) IR.Temp mRes
                    case (thType, elType) of
                        (_, TRecord{}) -> pure (elType, resOp)
                        _              -> pure (thType, resOp)
                Nothing -> do
                    checkUnit th
                    emitIR (IR.Label fLab)
                    pure (TUnit, IR.Const 0)
        While cond body _ ->  do
            begLab <- emitLabel
            (tLab, fLab) <- (,) <$> newLabel <*> newLabel
            withLoop fLab $ do
                transExprCond tLab fLab cond
                emitIR (IR.Label tLab)
                checkUnit body
                emitIR (IR.Jump begLab)
                emitIR (IR.Label fLab)
                pure (TUnit, IR.Const 0)
        For var esc startExpr endExpr body _ -> do
            fromOp <- checkInt startExpr
            toOp <- checkInt endExpr
            (begLab, tLab, fLab) <- (,,) <$> newLabel <*> newLabel <*> newLabel
            withNewEnv $ withLoop fLab $ do
                access <- allocLocal esc
                insertVarType var (VarEntry (VarInfo TInt access))

                varOp <- accessToIR access
                let loc = accessToLocation access
                assign varOp fromOp loc
                emitIR (IR.Label begLab)
                valOp <- getVarValue (IR.Temp varOp) loc
                emitIR (IR.CJump IR.Lt valOp toOp tLab fLab)
                emitIR (IR.Label tLab)
                checkUnit body
                emitIR (IR.Jump begLab)
                emitIR (IR.Label fLab)
            pure (TUnit, undefined)
        Break _ -> getLoopEndLabel >>= \case
            Nothing           -> throwError BreakOutsideLoop
            Just loopEndLabel -> emitIR (IR.Jump loopEndLabel) $> (TUnit, IR.Const 0)
        Seq es _ -> foldM (\_ e -> transExpr e) (TUnit, IR.Const 0) es
        Call funName args _ -> getFunInfo funName >>= \case
            FunInfo{..}
              | length args == length funInfoArgs -> do
                  argOps <- forM (zip args funInfoArgs) $ \(arg, expArgType) -> do
                      actExpArgType <- actualType expArgType
                      (argType, argOp) <- transExpr arg
                      unless (isTypesMatch actExpArgType argType) $
                          throwErrorPos (TypeMismatch actExpArgType argType)
                                        (exprSpan arg)
                      pure argOp
                  mDst <- if funInfoRet == TUnit then pure Nothing else Just <$> newTemp
                  frame <- getCurrentFrame
                  let isRec = Frame.frameName frame == funInfoLabel
                  argOps' <- if HashMap.member funName (libFunctions @f)
                                then pure argOps
                                else if not isRec
                                        then pure (IR.Temp FP : argOps)
                                        else (:) <$> getStaticLink frame <*> pure argOps
                  emitIR (IR.Call mDst funInfoLabel argOps')
                  let retOp = maybe (IR.Const 0) IR.Temp mDst
                  (,retOp) <$> actualType funInfoRet
              | otherwise -> throwError $ ArgumentsNumberMismatch (length funInfoArgs)
                                                                  (length args)
        Let ds e _ -> withNewEnv $ do
            mapM_ transDec ds
            transExpr e
  where assign :: MonadSemant m f => Temp -> IR Operand -> LValLocation -> m f ()
        assign lvalOp rvalOp = \case
            InReg -> emitIR (IR.Assign lvalOp rvalOp)
            InMem -> do
                src <- operandToTemp rvalOp
                emitIR (IR.Store (IR.Temp lvalOp) src)

        getVarValue :: MonadSemant m f => IR Operand -> LValLocation -> m f (IR Operand)
        getVarValue op = \case
            InReg -> pure op
            InMem -> do
                res <- newTemp
                emitIR (IR.Load res op)
                pure $ IR.Temp res

transBinop :: MonadSemant m f => Binop -> Expr -> Expr -> m f (IR Operand)
transBinop op e1 e2 = case op of
    Add -> transArith IR.Add e1 e2
    Sub -> transArith IR.Sub e1 e2
    Mul -> transArith IR.Mul e1 e2
    Div -> transArith IR.Div e1 e2
    Lt  -> transRelopExpr IR.Lt
    Le  -> transRelopExpr IR.Le
    Gt  -> transRelopExpr IR.Gt
    Ge  -> transRelopExpr IR.Ge
    Ne  -> transRelopExpr IR.Ne
    Eq  -> transRelopExpr IR.Eq
    And -> do
        (t1Lab, t2Lab, fLab) <- (,,) <$> newLabel <*> newLabel <*> newLabel
        resLab <- newLabel
        res <- newTemp
        transExprCond t1Lab fLab e1
        emitIR (IR.Label t1Lab)
        transExprCond t2Lab fLab e2
        emitIR (IR.Label t2Lab)
        emitIR (IR.Assign res (IR.Const 1))
        emitIR (IR.Jump resLab)
        emitIR (IR.Label fLab)
        emitIR (IR.Assign res (IR.Const 0))
        emitIR (IR.Label resLab)
        pure (IR.Temp res)
    Or  -> do
        (f1Lab, f2Lab, tLab) <- (,,) <$> newLabel <*> newLabel <*> newLabel
        resLab <- newLabel
        res <- newTemp
        transExprCond tLab f1Lab e1
        emitIR (IR.Label f1Lab)
        transExprCond tLab f2Lab e2
        emitIR (IR.Label f2Lab)
        emitIR (IR.Assign res (IR.Const 0))
        emitIR (IR.Jump resLab)
        emitIR (IR.Label tLab)
        emitIR (IR.Assign res (IR.Const 1))
        emitIR (IR.Label resLab)
        pure (IR.Temp res)
  where transRelopExpr :: MonadSemant m f => IR.Relop -> m f (IR Operand)
        transRelopExpr irOp = do
            (tLab, fLab, resLab) <- (,,) <$> newLabel <*> newLabel <*> newLabel
            transRelop tLab fLab irOp e1 e2
            res <- newTemp
            emitIR (IR.Label fLab)
            emitIR (IR.Assign res (IR.Const 0))
            emitIR (IR.Jump resLab)
            emitIR (IR.Label tLab)
            emitIR (IR.Assign res (IR.Const 1))
            emitIR (IR.Label resLab)
            pure (IR.Temp res)

transExprCond :: MonadSemant m f => Label -> Label -> Expr -> m f ()
transExprCond tLab fLab = \case
    Binop e1 op e2 _ -> case op of
        Add -> transArith IR.Add e1 e2 >>= opToCond
        Sub -> transArith IR.Sub e1 e2 >>= opToCond
        Mul -> transArith IR.Mul e1 e2 >>= opToCond
        Div -> transArith IR.Div e1 e2 >>= opToCond
        Lt  -> transRelop tLab fLab IR.Lt e1 e2
        Le  -> transRelop tLab fLab IR.Le e1 e2
        Gt  -> transRelop tLab fLab IR.Gt e1 e2
        Ge  -> transRelop tLab fLab IR.Ge e1 e2
        Ne  -> transRelop tLab fLab IR.Ne e1 e2
        Eq  -> transRelop tLab fLab IR.Eq e1 e2
        And -> do
            t2Lab <- newLabel
            transExprCond t2Lab fLab e1
            emitIR (IR.Label t2Lab)
            transExprCond tLab fLab e2
        Or  -> do
            f2Lab <- newLabel
            transExprCond tLab f2Lab e1
            emitIR (IR.Label f2Lab)
            transExprCond tLab fLab e2
    expr -> checkInt expr >>= opToCond
  where opToCond op = emitIR (IR.CJump IR.Eq op (IR.Const 1) tLab fLab)

transRelop :: forall m f. MonadSemant m f
           => Label
           -> Label
           -> IR.Relop
           -> Expr
           -> Expr
           -> m f ()
transRelop tLab fLab op e1 e2 = case op of
    IR.Ne -> transEq
    IR.Eq -> transEq
    _     -> do
        op1 <- checkInt e1
        op2 <- checkInt e2
        emitIR (IR.CJump op op1 op2 tLab fLab)
  where transEq = do
            (e1Type, op1) <- transExpr e1
            (e2Type, op2) <- transExpr e2
            when (e1Type == TUnit) $
                throwErrorPos UnitComparison (exprSpan e1)
            when (e2Type == TUnit) $
                throwErrorPos UnitComparison (exprSpan e2)
            unless (isTypesMatch e1Type e2Type) $
                throwErrorPos (TypeMismatch e1Type e2Type) (exprSpan e2)
            if e1Type /= TString
               then emitIR (IR.CJump op op1 op2 tLab fLab)
               else do
                   res <- newTemp
                   emitIR $ Frame.externalCall (Proxy @f) (Just res) StringEqual
                       [op1, op2]
                   emitIR (IR.CJump op (IR.Temp res) (IR.Const 0) fLab tLab)

transArith :: MonadSemant m f => IR.Binop -> Expr -> Expr -> m f (IR Operand)
transArith op e1 e2 = do
    op1 <- checkInt e1
    op2 <- checkInt e2
    res <- newTemp
    emitIR (IR.Binop res op op1 op2)
    pure (IR.Temp res)

data LValLocation = InReg | InMem

transLVal :: forall m f. MonadSemant m f => LVal -> m f (Type, LValLocation, Temp)
transLVal = transLVal' False
  where transLVal' isNested = \case
            Var var _ -> do
                VarInfo{..} <- getVarInfo var
                op <- accessToIR varInfoAccess
                let loc = accessToLocation varInfoAccess
                (, loc, op) <$> actualType varInfoType
            Dot lval field _ -> transLVal' True lval >>= \case
                (TRecord _ fields _, _, recPtr) -> case lookupIndex field fields of
                    Just (typ, idx) -> do
                        let offset = IR.Const $ idx * wordSize (Proxy @f)
                        fieldOp <- newTemp
                        emitIR (IR.Binop fieldOp IR.Add (IR.Temp recPtr) offset)
                        resOp <- loadNested fieldOp
                        (, InMem, resOp) <$> actualType typ
                    Nothing -> throwError $ RecordHasNoField (getName lval) field
                _ -> throwError $ NotRecord (getName lval)
            Index lval index _ -> transLVal' True lval >>= \case
                (TArray typ _, _, arrPtr) -> do
                    indexOp <- checkInt index
                    valueOp <- newTemp
                    let ws = IR.Const $ wordSize (Proxy @f)
                    emitIR (IR.Binop valueOp IR.Mul indexOp ws)
                    emitIR (IR.Binop valueOp IR.Add (IR.Temp valueOp) (IR.Temp arrPtr))
                    resOp <- loadNested valueOp
                    (, InMem, resOp) <$> actualType typ
                _            -> throwError $ NotArray (getName lval)
            where loadNested op
                    | isNested  = do
                        res <- newTemp
                        emitIR (IR.Load res (IR.Temp op))
                        pure res
                    | otherwise = pure op

                  getName :: LVal -> Text
                  getName = \case
                      Var var _      -> var
                      Dot _ field _  -> field
                      Index lval _ _ -> getName lval

accessToLocation :: Frame f => Access f -> LValLocation
accessToLocation (Access _ frAccess) = case frAccess of
    Frame.InReg{}   -> InReg
    Frame.InFrame{} -> InMem

transDec :: MonadSemant m f => Dec -> m f ()
transDec = \case
    TypeDecs decs -> do
        typeRefs <- forM decs $ \TypeDec{..} -> do
            ref <- newTypeRef
            insertType typeName (TName ref)
            pure ref

        forM_ (NonEmpty.zip decs typeRefs) $ \(TypeDec{..}, ref) -> case typeBody of
            TypeAlias name span -> do
                setSpan span
                let getLastRef aliasRef = readTypeRef aliasRef >>= \case
                        Just (TName nextAliasRef) -> getLastRef nextAliasRef
                        _                         -> pure aliasRef
                typ <- getType name >>= \case
                    TName aliasRef -> do
                        lastAliasRef <- getLastRef aliasRef
                        if lastAliasRef == ref
                           then throwError (CycleTypeDec typeName)
                           else pure (TName lastAliasRef)
                    aliasType -> pure aliasType
                insertType typeName typ
                writeTypeRef ref typ
            RecordType fields _ -> do
                let typeCheckField (res, seen) RecordField{..}
                      | recFieldName `elem` seen
                      = throwErrorPos (DuplicatedRecordField recFieldName) recFieldSpan
                      | otherwise = do
                          setSpan recFieldSpan
                          typ <- getType recFieldType
                          pure ((recFieldName, typ):res, recFieldName:seen)
                typedFields <- reverse . fst <$> foldM typeCheckField ([], []) fields
                typ <- TRecord typeName typedFields <$> newUnique
                insertType typeName typ
                writeTypeRef ref typ
            ArrayType name span -> do
                setSpan span
                typ <- TArray <$> getType name <*> newUnique
                insertType typeName typ
                writeTypeRef ref typ
    VarDec name mtyp expr esc span -> do
        setSpan span
        (exprTyp, exprOp) <- transExpr expr
        varTyp <- case mtyp of
            Just typText -> do
                typ <- getActualType typText
                if isTypesMatch typ exprTyp
                   then pure typ
                   else throwErrorPos (TypeMismatch typ exprTyp) (exprSpan expr)
            Nothing
              | exprTyp == TUnit -> throwErrorPos UnitAssignment (exprSpan expr)
              | exprTyp == TNil  -> throwErrorPos NilAssignmentWithoutType (exprSpan expr)
              | otherwise        -> pure exprTyp
        access <- allocLocal esc
        varOp <- accessToIR access
        let Access _ frAccess = access
        case frAccess of
            Frame.InReg{} -> emitIR (IR.Assign varOp exprOp)
            Frame.InFrame{} -> do
                src <- operandToTemp exprOp
                emitIR (IR.Store (IR.Temp varOp) src)
        insertVarType name (VarEntry (VarInfo varTyp access))
    FunDecs decs -> do
        labels <- forM decs $ \FunDec{..} -> do
            setSpan funSpan
            resType <- maybe (pure TUnit) getActualType funResult
            argTypes <- forM funArgs $ \DecField{..} -> do
                setSpan decFieldSpan
                getActualType decFieldType
            label <- newFuncLabel funName
            insertVarType funName (FunEntry (FunInfo argTypes resType label))
            pure label

        forM_ (NonEmpty.zip decs labels) $ \(FunDec{..}, label) -> do
            setSpan funSpan
            expResType <- maybe (pure TUnit) getActualType funResult
            let argsEscapes = map decFieldEscape funArgs
            actResType <- withNewEnv $ withNewFrame label argsEscapes $
                \argAccesses -> do
                    forM_ (zip funArgs argAccesses) $ \(DecField{..}, argAccess) -> do
                        setSpan decFieldSpan
                        argType <- getActualType decFieldType
                        insertVarType decFieldName (VarEntry (VarInfo argType argAccess))
                    (bodTyp, bodyOp) <-transExpr funBody
                    let retVal = maybe Nothing (const (Just bodyOp)) funResult
                    emitIR (IR.Ret retVal)
                    pure bodTyp
            unless (isTypesMatch expResType actResType) $
                throwErrorPos (TypeMismatch expResType actResType) funSpan

operandToTemp :: MonadSemant m f => IR Operand -> m f Temp
operandToTemp = \case
    IR.Temp t -> pure t
    op        -> newTemp >>= \res -> emitIR (IR.Assign res op) $> res

checkInt :: MonadSemant m f => Expr -> m f (IR Operand)
checkInt expr = transExpr expr >>= \case
    (TInt, op) -> pure op
    (typ, _)   -> throwErrorPos (TypeMismatch TInt typ) (exprSpan expr)

checkUnit :: MonadSemant m f => Expr -> m f ()
checkUnit expr = transExpr expr >>= \case
    (TUnit, _) -> pure ()
    (typ, _)   -> throwErrorPos (TypeMismatch TUnit typ) (exprSpan expr)

throwErrorPos :: MonadSemant m f => SemantException -> Span -> m f a
throwErrorPos e s = setSpan s >> throwError e

getActualType :: MonadSemant m f => Text -> m f Type
getActualType typeName = getType typeName >>= actualType

actualType :: MonadSemant m f => Type -> m f Type
actualType (TName ref) = readTypeRef ref >>= \case
    Just typ -> actualType typ
    Nothing  -> throwError EmptyName
actualType typ = pure typ

lookupIndex :: Eq a => a -> [(a, b)] -> Maybe (b, Int)
lookupIndex = lookupIndex' 0
  where lookupIndex' idx k ((f, b) : xs)
          | k == f    = Just (b, idx)
          | otherwise = lookupIndex' (idx + 1) k xs
        lookupIndex' _ _ _ = Nothing
