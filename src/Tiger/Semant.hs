module Tiger.Semant
    ( Type(..)
    , Unique(..)
    , SemantException(..)
    , PosedSemantException(..)
    , posedExceptionToText
    , exceptionToText
    , semantAnalyze
    ) where

import           Control.Exception      (Exception, throwIO, try)
import           Control.Monad          (foldM, forM, forM_, unless, when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT)
import           Data.Functor           (($>))
import           Data.HashMap.Strict    (HashMap)
import           Data.IORef             (IORef, modifyIORef', newIORef, readIORef,
                                         writeIORef)
import           Data.List              (delete)
import           Data.Text              (Text)
import           Prelude                hiding (exp, span)

import           Tiger.Expr

import qualified Data.HashMap.Strict    as HashMap
import qualified Data.List.NonEmpty     as NonEmpty
import qualified Data.Text              as Text

data Type
    = TInt
    | TString
    | TRecord !Text ![(Text, Type)] !Unique
    | TArray !Type !Unique
    | TNil
    | TUnit
    | TName !TypeRef
    deriving Eq

isTypesMacth :: Type -> Type -> Bool
isTypesMacth TInt TInt                         = True
isTypesMacth TString TString                   = True
isTypesMacth TRecord{} TNil                    = True
isTypesMacth TNil TRecord{}                    = True
isTypesMacth (TRecord _ _ u1) (TRecord _ _ u2) = u1 == u2
isTypesMacth (TArray _ u1) (TArray _ u2)       = u1 == u2
isTypesMacth TNil TNil                         = True
isTypesMacth TUnit TUnit                       = True
isTypesMacth (TName r1) (TName r2)             = r1 == r2
isTypesMacth _ _                               = False

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

newtype SemantM a = SemantM { runSemantM :: ReaderT Context IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader Context, MonadIO)

data Context = Context
    { ctxUnique :: !(IORef Unique)
    , ctxEnvs   :: !(IORef [(HashMap Text EnvEntry, HashMap Text Type)])
    , ctxSpan   :: !(IORef Span)
    , ctxInLoop :: !(IORef Bool)
    }

data EnvEntry
    = VarEntry !Type
    | FunEntry ![Type] !Type

data EnvRequestType
    = ReqVar
    | ReqFun

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

instance MonadSemant SemantM where
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
    getVarType name reqType = do
        let err = case reqType of
                ReqFun -> UndefinedFunction
                ReqVar -> UndefinedVariable
        asks ctxEnvs >>= liftIO . readIORef >>= \case
            (varEnv, _):_ -> case HashMap.lookup name varEnv of
                Just entry -> pure entry
                Nothing    -> liftIO $ throwIO $ err name
            _ -> liftIO $ throwIO $ err name
    getType name = do
        asks ctxEnvs >>= liftIO . readIORef >>= \case
            (_, typeEnv):_ -> case HashMap.lookup name typeEnv of
                Just entry -> pure entry
                Nothing    -> liftIO $ throwIO $ UndefinedType name
            _ -> liftIO $ throwIO $ UndefinedType name
    withLoop f = do
        inLoopRef <- asks ctxInLoop
        initValue <- liftIO $ readIORef inLoopRef
        liftIO $ writeIORef inLoopRef True
        res <- f
        liftIO $ writeIORef inLoopRef initValue
        pure res
    getIsInLoop = asks ctxInLoop >>= liftIO . readIORef
    setSpan s = asks ctxSpan >>= \spanRef -> liftIO $ writeIORef spanRef s
    throwError = liftIO . throwIO

class Monad m => MonadSemant m where
    newUnique     :: m Unique
    newTypeRef    :: m TypeRef
    readTypeRef   :: TypeRef -> m (Maybe Type)
    writeTypeRef  :: TypeRef -> Type -> m ()
    withNewEnv    :: m a -> m a
    insertVarType :: Text -> EnvEntry -> m ()
    insertType    :: Text -> Type -> m ()
    getVarType    :: Text -> EnvRequestType -> m EnvEntry
    getType       :: Text -> m Type
    withLoop      :: m a -> m a
    getIsInLoop   :: m Bool
    setSpan       :: Span -> m ()
    throwError    :: SemantException -> m a

semantAnalyze :: FilePath -> Expr -> IO (Either PosedSemantException ())
semantAnalyze file expr = do
    uniqueRef <- newIORef (Unique 0)
    envsRef <- newIORef [(initFunctions, initTypes)]
    spanRef <- newIORef (Span (Position 0 0) (Position 0 0))
    inLoopRef <- newIORef False
    let ctx = Context uniqueRef envsRef spanRef inLoopRef
    let eval :: IO (Either SemantException Type)
        eval = try (runReaderT (runSemantM $ transExpr expr) ctx)
    eval >>= \case
        Left err -> do
            span <- readIORef spanRef
            pure (Left $ PosedSemantException file err span)
        Right{}  -> pure (Right ())
  where initFunctions = HashMap.fromList
            [ ("print", FunEntry [TString] TUnit)
            , ("flush", FunEntry [] TUnit)
            , ("getchar", FunEntry [] TString)
            , ("ord", FunEntry [TString] TInt)
            , ("chr", FunEntry [TInt] TString)
            , ("size", FunEntry [TString] TInt)
            , ("substring", FunEntry [TString, TInt, TInt] TString)
            , ("concat", FunEntry [TString, TString] TString)
            , ("not", FunEntry [TInt] TInt)
            , ("exit", FunEntry [TInt] TUnit)
            ]
        initTypes = HashMap.fromList [("int", TInt), ("string", TString)]

transExpr :: MonadSemant m => Expr -> m Type
transExpr expr = do
    setSpan (exprSpan expr)
    case expr of
        LVal lval -> transLVal lval
        Nil _ -> pure TNil
        IntLit{} -> pure TInt
        StrLit{} -> pure TString
        Neg e _ -> checkInt e $> TInt
        Binop e1 op e2 _ -> do
            let checkEq = do
                    e1Type <- transExpr e1
                    e2Type <- transExpr e2
                    when (e1Type == TUnit) $
                        throwErrorPos UnitComparison (exprSpan e1)
                    when (e2Type == TUnit) $
                        throwErrorPos UnitComparison (exprSpan e2)
                    unless (isTypesMacth e1Type e2Type) $
                        throwErrorPos (TypeMismatch e1Type e2Type) (exprSpan e2)
                    pure TInt
            case op of
                Eq -> checkEq
                Ne -> checkEq
                _  -> checkInt e1 >> checkInt e2 $> TInt
        Record typeName fields _ -> getActualType typeName >>= \case
            typ@(TRecord _ typeFields _) -> do
                let checkField (notSeenFields, seenFields) Field{..}
                      | fieldName `elem` seenFields
                      = throwErrorPos (DuplicatedRecordField fieldName) fieldSpan
                      | Just fieldType <- lookup fieldName typeFields
                      = do
                          fieldValueType <- transExpr fieldValue
                          actFieldType <- actualType fieldType
                          unless (isTypesMacth actFieldType fieldValueType) $
                              throwErrorPos (TypeMismatch actFieldType fieldValueType)
                                            fieldSpan
                          pure (delete fieldName notSeenFields, fieldName:seenFields)
                      | otherwise
                      = throwError (RecordHasNoField typeName fieldName)
                fst <$> foldM checkField (map fst typeFields, []) fields >>= \case
                    (f:_) -> throwError (UnitializedRecordField typeName f)
                    _     -> pure typ
            typ -> throwError (NotRecordType typ)
        Array typeName sizeExpr initExpr _ -> getActualType typeName >>= \case
            typ@(TArray elemType _) -> do
                actElemType <- actualType elemType
                checkInt sizeExpr
                initExprType <- transExpr initExpr
                unless (isTypesMacth actElemType initExprType) $
                    throwError (TypeMismatch actElemType initExprType)
                pure typ
            typ -> throwError (NotArrayType typ)
        Assign lval rval _ -> do
            lvalType <- transLVal lval
            rvalType <- transExpr rval
            unless (isTypesMacth lvalType rvalType) $
                throwErrorPos (TypeMismatch lvalType rvalType) (exprSpan rval)
            pure TUnit
        If cond th mel _ -> do
            checkInt cond
            case mel of
                Just el -> do
                    thType <- transExpr th
                    elType <- transExpr el
                    unless (isTypesMacth thType elType) $
                        throwError (TypeMismatch thType elType)
                    case (thType, elType) of
                        (_, TRecord{}) -> pure elType
                        _              -> pure thType
                Nothing -> checkUnit th $> TUnit
        While cond body _ -> withLoop $ do
            checkInt cond
            checkUnit body
            pure TUnit
        For var _ startExpr endExpr body _ -> do
            checkInt startExpr
            checkInt endExpr
            withNewEnv $ withLoop $ do
                insertVarType var (VarEntry TInt)
                checkUnit body
            pure TUnit
        Seq es _ -> foldM (\_ e -> transExpr e) TUnit es
        Call funName args _ -> getVarType funName ReqFun >>= \case
            FunEntry expArgTypes resType
              | length args == length expArgTypes -> do
                  forM_ (zip args expArgTypes) $ \(arg, expArgType) -> do
                      actExpArgType <- actualType expArgType
                      argType <- transExpr arg
                      unless (isTypesMacth actExpArgType argType) $
                          throwErrorPos (TypeMismatch actExpArgType argType)
                                        (exprSpan arg)
                  actualType resType
              | otherwise -> throwError $ ArgumentsNumberMismatch (length expArgTypes)
                                                                  (length args)
            _ -> throwError (UndefinedFunction funName)
        Break _ -> do
            isInLoop <- getIsInLoop
            unless isInLoop (throwError BreakOutsideLoop)
            pure TUnit
        Let ds e _ -> withNewEnv $ do
            mapM_ transDec ds
            transExpr e

transLVal :: MonadSemant m => LVal -> m Type
transLVal = \case
    Var var _ -> getVarType var ReqVar >>= \case
        VarEntry typ -> actualType typ
        FunEntry{}   -> throwError $ UndefinedVariable var
    Dot lval field _ -> transLVal lval >>= \case
        TRecord _ fields _ -> case lookup field fields of
            Just typ -> actualType typ
            Nothing  -> throwError $ RecordHasNoField (getName lval) field
        _ -> throwError $ NotRecord (getName lval)
    Index lval index _ -> transLVal lval >>= \case
        TArray typ _ -> checkInt index >> actualType typ
        _            -> throwError $ NotArray (getName lval)
  where getName :: LVal -> Text
        getName = \case
            Var var _      -> var
            Dot _ field _  -> field
            Index lval _ _ -> getName lval

transDec :: MonadSemant m => Dec -> m ()
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
                let typeCheckField (res, seen) DecField{..}
                      | decFieldName `elem` seen
                      = throwErrorPos (DuplicatedRecordField decFieldName) decFieldSpan
                      | otherwise = do
                          setSpan decFieldSpan
                          typ <- getType decFieldType
                          pure ((decFieldName, typ):res, decFieldName:seen)
                typedFields <- reverse . fst <$> foldM typeCheckField ([], []) fields
                typ <- TRecord typeName typedFields <$> newUnique
                insertType typeName typ
                writeTypeRef ref typ
            ArrayType name span -> do
                setSpan span
                typ <- TArray <$> getType name <*> newUnique
                insertType typeName typ
                writeTypeRef ref typ
    VarDec name mtyp expr _ span -> do
        setSpan span
        exprTyp <- transExpr expr
        varTyp <- case mtyp of
            Just typText -> do
                typ <- getActualType typText
                if isTypesMacth typ exprTyp
                   then pure typ
                   else throwErrorPos (TypeMismatch typ exprTyp) (exprSpan expr)
            Nothing
              | exprTyp == TUnit -> throwErrorPos UnitAssignment (exprSpan expr)
              | exprTyp == TNil  -> throwErrorPos NilAssignmentWithoutType (exprSpan expr)
              | otherwise        -> pure exprTyp
        insertVarType name (VarEntry varTyp)
    FunDecs decs -> do
        forM_ decs $ \FunDec{..} -> do
            setSpan funSpan
            resType <- maybe (pure TUnit) getActualType funResult
            argTypes <- forM funArgs $ \DecField{..} -> do
                setSpan decFieldSpan
                getActualType decFieldType
            insertVarType funName (FunEntry argTypes resType)

        forM_ decs $ \FunDec{..} -> do
            setSpan funSpan
            expResType <- maybe (pure TUnit) getActualType funResult
            actResType <- withNewEnv $ do
                forM_ funArgs $ \DecField{..} -> do
                    setSpan decFieldSpan
                    argType <- getActualType decFieldType
                    insertVarType decFieldName (VarEntry argType)
                transExpr funBody
            unless (isTypesMacth expResType actResType) $
                throwErrorPos (TypeMismatch expResType actResType) funSpan

checkInt :: MonadSemant m => Expr -> m ()
checkInt expr = transExpr expr >>= \case
    TInt -> pure ()
    typ  -> throwErrorPos (TypeMismatch TInt typ) (exprSpan expr)

checkUnit :: MonadSemant m => Expr -> m ()
checkUnit expr = transExpr expr >>= \case
    TUnit -> pure ()
    typ   -> throwErrorPos (TypeMismatch TUnit typ) (exprSpan expr)

throwErrorPos :: MonadSemant m => SemantException -> Span -> m a
throwErrorPos e s = setSpan s >> throwError e

getActualType :: MonadSemant m => Text -> m Type
getActualType typeName = getType typeName >>= actualType

actualType :: MonadSemant m => Type -> m Type
actualType (TName ref) = readTypeRef ref >>= \case
    Just typ -> actualType typ
    Nothing  -> throwError EmptyName
actualType typ = pure typ
