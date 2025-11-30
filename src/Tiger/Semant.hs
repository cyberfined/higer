module Tiger.Semant
    ( Type(..)
    , SemantException(..)
    , PosedSemantException(..)
    , semantAnalyze
    ) where

import           Control.Exception          (Exception)
import           Control.Monad              (foldM, forM, forM_, mapAndUnzipM, unless,
                                             when)
import           Control.Monad.Except       (ExceptT (..), MonadError (..), runExceptT,
                                             throwError)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.Trans.Class  (lift)
import           Data.HashMap.Strict        (HashMap)
import           Data.IORef                 (IORef, modifyIORef', newIORef, readIORef,
                                             writeIORef)
import           Data.List                  (delete)
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import           Data.Text.Lazy.Builder     (fromString, fromText)
import           Data.Vector.Hashtables     (Dictionary, PrimMonad (PrimState))
import           Prelude                    hiding (exp, span)

import           Tiger.EscapeAnalysis       (EscapeAnalysisResult,
                                             getEscapeAnalysisResult)
import           Tiger.Expr
import           Tiger.Frame                (Frame)
import           Tiger.IR.Types             (IR, IRData (..), IRFunction,
                                             LabeledString (..), Stmt)
import           Tiger.Semant.LibFunctions  (LibFunction (..))
import           Tiger.Semant.Type
import           Tiger.Temp
import           Tiger.TextUtils
import           Tiger.Translate
import           Tiger.Unique

import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashSet               as HashSet
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Vector.Hashtables     as HashTable
import qualified Data.Vector.Mutable        as MVec

import qualified Tiger.Semant.LibFunctions  as LibFunctions

type HashTable k v = Dictionary (PrimState IO) MVec.MVector k MVec.MVector v

newtype SemantM m f a = SemantM
    { runSemantM :: ReaderT (Context f) (ExceptT SemantException m) a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadReader (Context f)
                       , MonadIO
                       , MonadError SemantException
                       )

data Context f = Context
    { ctxUnique       :: !(IORef Unique)
    , ctxEnvs         :: !(IORef [(HashMap Text (EnvEntry f), HashMap Text Type)])
    , ctxSpan         :: !(IORef Span)
    , ctxLoopEndLabel :: !(IORef (Maybe Label))
    , ctxCurrentLevel :: !(IORef (Level f))
    , ctxStrings      :: !(HashTable Text Label)
    , ctxIRFuncs      :: !(IORef [IRFunction Stmt f])
    }

data EnvEntry f
    = VarEntry !(VarInfo f)
    | FunEntry !(FunInfo f)

data VarInfo f = VarInfo
    { varInfoType   :: !Type
    , varInfoAccess :: !(Access f)
    }

data FunInfo f = FunInfo
    { funInfoArgs        :: ![Type]
    , funInfoRet         :: !Type
    , funInfoLabel       :: !Label
    , funInfoParentLevel :: Level f
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
    | DuplicatedFunctionDefinition !Text
    deriving Show

instance TextBuildable SemantException where
  toTextBuilder = \case
    UndefinedVariable var            -> "undefined variable " <> fromText var
    UndefinedType typ                -> "undefined type " <> fromText typ
    RecordHasNoField rec field       -> "record " <> fromText rec <> " has no field " <> fromText field
    NotRecord lval                   -> fromText lval <> " is not a record"
    NotArray lval                    -> fromText lval <> " is not an array"
    TypeMismatch exp act             -> "type mismatch: expecting " <> toTextBuilder exp
                                     <> ", actual " <> toTextBuilder act
    DuplicatedRecordField field      -> "duplicated record field " <> fromText field
    CycleTypeDec typ                 -> "type " <> fromText typ <> " forms cycle"
    EmptyName                        -> "name reference is empty"
    NotRecordType typ                -> toTextBuilder typ <> " is not a record type"
    UnitializedRecordField rec field -> "field " <> fromText field <> " of " <> fromText rec
                                     <> " is unitialized"
    NotArrayType typ                 -> toTextBuilder typ <> " is not an array type"
    BreakOutsideLoop                 -> "break is outside the loop"
    UnitAssignment                   -> "can't assign unit type expression"
    UndefinedFunction fun            -> "undefined function " <> fromText fun
    ArgumentsNumberMismatch exp act  -> "wrong number of arguments: expecting "
                                     <> Builder.decimal exp <> ", actual "
                                     <> Builder.decimal act
    UnitComparison                   -> "unit type expressions can't be compared"
    NilAssignmentWithoutType         -> "can't assign nil without type constraint"
    DuplicatedFunctionDefinition fun -> "duplicated definition of function " <> fromText fun

instance Exception SemantException

data PosedSemantException = PosedSemantException !FilePath !SemantException !Span

instance TextBuildable PosedSemantException where
  toTextBuilder (PosedSemantException file err span)
    =  fromString file <> ":" <> Builder.decimal l1 <> ":" <> Builder.decimal c1
    <> "-" <> Builder.decimal l2 <> ":" <> Builder.decimal c2
    <> ": " <> toTextBuilder err
    where Span (Position l1 c1) (Position l2 c2) = span

instance (Frame f, MonadIO m) => MonadUnique (SemantM m f) where
    newUnique = do
        uniqueRef <- asks ctxUnique
        uniq@(Unique cur) <- liftIO $ readIORef uniqueRef
        liftIO $ writeIORef uniqueRef $ Unique (cur + 1)
        pure uniq

instance (Frame f, MonadIO m, MonadTemp m) => MonadSemant (SemantM m) f where
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
    getEnvEntry name = asks ctxEnvs >>= liftIO . readIORef >>= pure . \case
        (env, _):_ -> HashMap.lookup name env
        _          -> Nothing
    getType name = do
        asks ctxEnvs >>= liftIO . readIORef >>= \case
            (_, typeEnv):_ -> case HashMap.lookup name typeEnv of
                Just entry -> pure entry
                Nothing    -> throwError $ UndefinedType name
            _ -> throwError $ UndefinedType name
    withLoop f = do
        loopEndLabelRef <- asks ctxLoopEndLabel
        initValue <- liftIO $ readIORef loopEndLabelRef
        fLab <- newLabel
        liftIO $ writeIORef loopEndLabelRef (Just fLab)
        res <- f fLab
        liftIO $ writeIORef loopEndLabelRef initValue
        pure res
    getLoopEndLabel = asks ctxLoopEndLabel >>= liftIO . readIORef
    setSpan s = asks ctxSpan >>= \spanRef -> liftIO $ writeIORef spanRef s

instance (Frame f, MonadTemp m) => MonadTemp (SemantM m f) where
    newTemp = SemantM $ lift $ lift newTemp
    newLabel = SemantM $ lift $ lift newLabel

instance (Frame f, MonadIO m, MonadTemp m) => MonadTranslate (SemantM m) f where
    getCurrentLevel = asks ctxCurrentLevel >>= liftIO . readIORef

    setCurrentLevel level = do
        currentLevelRef <- asks ctxCurrentLevel
        liftIO $ writeIORef currentLevelRef level

    insertString str label = do
        stringsMap <- asks ctxStrings
        liftIO $ HashTable.insert stringsMap str label

    lookupString str = do
        stringsMap <- asks ctxStrings
        liftIO $ HashTable.lookup stringsMap str

    insertIRFunction func = do
        irFuncsRef <- asks ctxIRFuncs
        liftIO $ modifyIORef' irFuncsRef (func:)

class (Frame f, MonadError SemantException (m f), MonadUnique (m f)) => MonadSemant m f where
    newTypeRef      :: m f TypeRef
    readTypeRef     :: TypeRef -> m f (Maybe Type)
    writeTypeRef    :: TypeRef -> Type -> m f ()
    withNewEnv      :: m f a -> m f a
    insertVarType   :: Text -> EnvEntry f -> m f ()
    insertType      :: Text -> Type -> m f ()
    getEnvEntry     :: Text -> m f (Maybe (EnvEntry f))
    getType         :: Text -> m f Type
    withLoop        :: (Label -> m f a) -> m f a
    getLoopEndLabel :: m f (Maybe Label)
    setSpan         :: Span -> m f ()

semantAnalyze :: forall f m. (Frame f, MonadIO m, MonadTemp m)
              => FilePath
              -> EscapeAnalysisResult
              -> m (Either PosedSemantException (IRData Stmt f))
semantAnalyze file expr = do
    uniqueRef <- liftIO $ newIORef (Unique 0)
    envsRef <- liftIO $ newIORef [(libFunctions, initTypes)]
    spanRef <- liftIO $ newIORef (Span (Position 0 0) (Position 0 0))
    loopEndLabelRef <- liftIO $ newIORef Nothing
    currentLevelRef <- liftIO $ newIORef undefined
    stringsMap <- liftIO $ HashTable.initialize 64
    irFuncsRef <- liftIO $ newIORef []
    let ctx = Context
            { ctxUnique       = uniqueRef
            , ctxEnvs         = envsRef
            , ctxSpan         = spanRef
            , ctxLoopEndLabel = loopEndLabelRef
            , ctxCurrentLevel = currentLevelRef
            , ctxStrings      = stringsMap
            , ctxIRFuncs      = irFuncsRef
            }
    let transMain = withMainFrame (transExpr @f (getEscapeAnalysisResult expr))
    runExceptT (runReaderT (runSemantM transMain) ctx) >>= \case
        Left err -> do
            span <- liftIO $ readIORef spanRef
            pure (Left $ PosedSemantException file err span)
        Right{} -> do
            strings <- map (uncurry LabeledString) <$> liftIO (HashTable.toList stringsMap)
            functions <- liftIO $ readIORef irFuncsRef
            pure $ Right $ IRData strings functions
  where initTypes = HashMap.fromList [("int", TInt), ("string", TString)]
        libFunctions = HashMap.fromList $ map libFunToEntry LibFunctions.libFunctions
        libFunToEntry LibFunction{..} =
            let info = FunInfo libFunArgs libFunRet (LabelText libFunName) undefined
            in (libFunName, FunEntry info)

transExpr :: (Frame f, MonadSemant m f, MonadTranslate m f)
          => Expr
          -> m f (Type, IR)
transExpr expr = do
    setSpan (exprSpan expr)
    case expr of
        LVal lval  -> transLVal lval
        Nil _      -> pure (TNil, irNil)
        IntLit i _ -> pure (TInt, irInt i)
        StrLit s _ -> irString s >>= \ir -> pure (TString, ir)
        Neg e _ -> do
            ir <- checkInt e
            (TInt,) <$> transNeg ir
        Binop e1 op e2 _ -> do
            let checkEq = do
                    (e1Type, ir1) <- transExpr e1
                    (e2Type, ir2) <- transExpr e2
                    when (e1Type == TUnit) $
                        throwErrorPos UnitComparison (exprSpan e1)
                    when (e2Type == TUnit) $
                        throwErrorPos UnitComparison (exprSpan e2)
                    unless (isTypesMatch e1Type e2Type) $
                        throwErrorPos (TypeMismatch e1Type e2Type) (exprSpan e2)
                    (TInt,) <$> transBinop op e1Type ir1 ir2
            case op of
                Eq -> checkEq
                Ne -> checkEq
                _  -> do
                    ir1 <- checkInt e1
                    ir2 <- checkInt e2
                    (TInt,) <$> transBinop op TInt ir1 ir2
        Record typeName fields _ -> getActualType typeName >>= \case
            typ@(TRecord _ typeFields _) -> do
                let checkFields notSeenFields seenFields values (Field{..}:fs)
                      | fieldName `elem` seenFields
                      = throwErrorPos (DuplicatedRecordField fieldName) fieldSpan
                      | Just (fieldType, fieldIdx) <- lookupIndex fieldName typeFields
                      = do
                          (fieldValueType, fieldValueIr) <- transExpr fieldValue
                          actFieldType <- actualType fieldType
                          unless (isTypesMatch actFieldType fieldValueType) $
                              throwErrorPos (TypeMismatch actFieldType fieldValueType)
                                            fieldSpan
                          checkFields (delete fieldName notSeenFields)
                                      (fieldName : seenFields)
                                      ((fieldValueIr, fieldIdx) : values)
                                      fs
                      | otherwise
                      = throwError (RecordHasNoField typeName fieldName)
                    checkFields (f:_) _ _ [] =
                        throwError (UnitializedRecordField typeName f)
                    checkFields _ _ values [] = transRecord (reverse values)
                (typ, ) <$> checkFields (map fst typeFields) [] [] fields
            typ -> throwError (NotRecordType typ)
        Array typeName sizeExpr initExpr _ -> getActualType typeName >>= \case
            typ@(TArray elemType _) -> do
                actElemType <- actualType elemType
                sizeIr <- checkInt sizeExpr
                (initType, initIr) <- transExpr initExpr
                unless (isTypesMatch actElemType initType) $
                    throwError (TypeMismatch actElemType initType)
                (typ,) <$> transArray sizeIr initIr
            typ -> throwError (NotArrayType typ)
        Assign lval rval _ -> do
            (lvalType, lvalIr) <- transLVal lval
            (rvalType, rvalIr) <- transExpr rval
            unless (isTypesMatch lvalType rvalType) $
                throwErrorPos (TypeMismatch lvalType rvalType) (exprSpan rval)
            (TUnit,) <$> transAssign lvalIr rvalIr
        If cond th mel _ -> do
            condIr <- checkInt cond
            case mel of
                Just el -> do
                    (thType, thIr) <- transExpr th
                    (elType, elIr) <- transExpr el
                    unless (isTypesMatch thType elType) $
                        throwError (TypeMismatch thType elType)
                    let resType = case (thType, elType) of
                            (_, TRecord{}) -> elType
                            _              -> thType
                    (resType, ) <$> transIfElse resType condIr thIr elIr
                Nothing -> do
                    thIr <- checkUnit th
                    (TUnit, ) <$> transIf condIr thIr
        While cond body _ -> withLoop $ \fLab -> do
            condIr <- checkInt cond
            bodyIr <- checkUnit body
            (TUnit,) <$> transWhile condIr bodyIr fLab
        For var esc startExpr endExpr body _ -> do
            startIr <- checkInt startExpr
            endIr <- checkInt endExpr
            withNewEnv $ withLoop $ \fLab -> do
                access <- allocLocal esc
                insertVarType var (VarEntry $ VarInfo TInt access)
                bodyIr <- checkUnit body
                (TUnit, ) <$> transFor access startIr endIr bodyIr fLab
        Break _ -> getLoopEndLabel >>= \case
            Nothing   -> throwError BreakOutsideLoop
            Just fLab -> pure (TUnit, transBreak fLab)
        Seq es _ -> do
            (esTypes, esIr) <- mapAndUnzipM transExpr es
            let resTyp = if null esTypes then TUnit else last esTypes
            (resTyp,) <$> transSeq resTyp esIr
        Call funName args _ -> getFunInfo funName >>= \case
            FunInfo{..}
                | length args == length funInfoArgs -> do
                      argsIR <- forM (zip args funInfoArgs) $ \(arg, expArgType) -> do
                          actExpArgType <- actualType expArgType
                          (argType, argIR) <- transExpr arg
                          unless (isTypesMatch actExpArgType argType) $
                              throwErrorPos (TypeMismatch actExpArgType argType)
                                            (exprSpan arg)
                          pure argIR
                      retType <- actualType funInfoRet
                      retIR <- transCall funName funInfoLabel funInfoParentLevel argsIR
                      pure (retType, retIR)
                | otherwise -> throwError $ ArgumentsNumberMismatch (length funInfoArgs)
                                                                    (length args)
        Let ds e _ -> withNewEnv $ do
            irs <- catMaybes . NonEmpty.toList <$> mapM transDec ds
            (typ, ir) <- transExpr e
            (typ,) <$> transLet irs ir

transLVal :: (Frame f, MonadSemant m f, MonadTranslate m f)
          => LVal
          -> m f (Type, IR)
transLVal = \case
    Var var _ -> do
        VarInfo{..} <- getVarInfo var
        op <- transVar varInfoAccess
        (, op) <$> actualType varInfoType
    Dot lval field _ -> transLVal lval >>= \case
        (TRecord _ fields _, recPtr) -> case lookupIndex field fields of
            Just (typ, idx) -> do
                op <- transDot recPtr idx
                (, op) <$> actualType typ
            Nothing -> throwError $ RecordHasNoField (getName lval) field
        _ -> throwError $ NotRecord (getName lval)
    Index lval index _ -> transLVal lval >>= \case
        (TArray typ _, arrPtr) -> do
            indexIr <- checkInt index
            (,) <$> actualType typ <*> transIndex arrPtr indexIr
        _            -> throwError $ NotArray (getName lval)
  where getName :: LVal -> Text
        getName = \case
            Var var _      -> var
            Dot _ field _  -> field
            Index lval _ _ -> getName lval

transDec :: (Frame f, MonadSemant m f, MonadTranslate m f) => Dec -> m f (Maybe IR)
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

        pure Nothing
    VarDec name mtyp expr esc span -> do
        setSpan span
        (exprTyp, ir) <- transExpr expr
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
        insertVarType name (VarEntry $ VarInfo varTyp access)
        Just <$> transVarDec ir access
    FunDecs decs -> do
        let insertFuncs (FunDec{..}:ds) funSet fs = do
                setSpan funSpan
                when (HashSet.member funName funSet) $
                    throwError $ DuplicatedFunctionDefinition funName
                resType <- maybe (pure TUnit) getActualType funResult
                argTypes <- forM funArgs $ \DecField{..} -> do
                    setSpan decFieldSpan
                    getActualType decFieldType
                label <- newFuncLabel funName
                level <- getCurrentLevel
                let funInfo = FunInfo argTypes resType label level
                insertVarType funName (FunEntry funInfo)
                insertFuncs ds (HashSet.insert funName funSet) (funInfo:fs)
            insertFuncs _ _ fs = pure $ NonEmpty.fromList $ reverse fs
        funInfos <- insertFuncs (NonEmpty.toList decs) HashSet.empty []

        forM_ (NonEmpty.zip decs funInfos) $ \(FunDec{..}, FunInfo{..}) -> do
            setSpan funSpan
            let argEscapes = map decFieldEscape funArgs
            (actResType, _) <- withNewEnv $
                withNewFrame funInfoLabel argEscapes $ \argAccesses -> do
                    forM_ (zip funArgs argAccesses) $ \(DecField{..}, argAccess) -> do
                        setSpan decFieldSpan
                        argType <- getActualType decFieldType
                        insertVarType decFieldName (VarEntry (VarInfo argType argAccess))
                    transExpr funBody
            unless (isTypesMatch funInfoRet actResType) $
                throwErrorPos (TypeMismatch funInfoRet actResType) funSpan
        pure Nothing

checkInt :: (Frame f, MonadSemant m f, MonadTranslate m f) => Expr -> m f IR
checkInt expr = transExpr expr >>= \case
    (TInt, ir) -> pure ir
    (typ, _)   -> throwErrorPos (TypeMismatch TInt typ) (exprSpan expr)

checkUnit :: (Frame f, MonadSemant m f, MonadTranslate m f) => Expr -> m f IR
checkUnit expr = transExpr expr >>= \case
    (TUnit, ir) -> pure ir
    (typ, _)    -> throwErrorPos (TypeMismatch TUnit typ) (exprSpan expr)

throwErrorPos :: (Frame f, MonadSemant m f) => SemantException -> Span -> m f a
throwErrorPos e s = setSpan s >> throwError e

getActualType :: (Frame f, MonadSemant m f) => Text -> m f Type
getActualType typeName = getType typeName >>= actualType

actualType :: (Frame f, MonadSemant m f) => Type -> m f Type
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

getVarInfo :: (Frame f, MonadSemant m f) => Text -> m f (VarInfo f)
getVarInfo name = getEnvEntry name >>= \case
    Just (VarEntry varInfo) -> pure varInfo
    _                       -> throwError $ UndefinedVariable name

getFunInfo :: (Frame f, MonadSemant m f) => Text -> m f (FunInfo f)
getFunInfo name = getEnvEntry name >>= \case
    Just (FunEntry funInfo) -> pure funInfo
    _                       -> throwError $ UndefinedFunction name
