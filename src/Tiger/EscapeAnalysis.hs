module Tiger.EscapeAnalysis (escapeAnalyze) where

import           Control.Monad          (foldM, forM, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, local, runReaderT)
import           Data.HashMap.Strict    (HashMap)
import           Data.IORef             (IORef, modifyIORef', newIORef, readIORef)
import           Data.Text              (Text)
import           Prelude                hiding (span)

import           Tiger.Expr

import qualified Data.HashMap.Strict    as HashMap
import qualified Data.List.NonEmpty     as NonEmpty

newtype EscapeM a = EscapeM { runEscapeM :: ReaderT Context IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader Context, MonadIO)

data Context = Context
    { ctxEnv   :: !(HashMap Text EntryRef)
    , ctxDepth :: !Depth
    }

newtype Depth = Depth Int deriving newtype Eq

newtype EntryRef = EntryRef (IORef Entry)

data Entry = Entry
    { entryEsc   :: !Escaping
    , entryDepth :: !Depth
    }

class Monad m => MonadEscape m where
    withLet      :: [Text] -> m a -> m ([EntryRef], a)
    withFunction :: [Text] -> m a -> m ([EntryRef], a)
    getDepth     :: m Depth
    getVar       :: Text -> m (Maybe EntryRef)
    getVarDepth  :: EntryRef -> m Depth
    getVarEsc    :: EntryRef -> m Escaping
    markVarEsc   :: EntryRef -> m ()

instance MonadEscape EscapeM where
    withLet vs f = do
        depth <- asks ctxDepth
        refs <- mapM (\_ -> EntryRef <$> liftIO (newIORef $ Entry Remaining depth)) vs
        let modifyEnv env (var, ref) = HashMap.insert var ref env
        let modifyCtx ctx@Context{..} =
                ctx { ctxEnv = foldl modifyEnv ctxEnv (zip vs refs) }
        local modifyCtx $ (,) <$> pure refs <*> f
    withFunction vs f = local modifyCtx $ withLet vs f
      where modifyCtx ctx@Context{..} =
                let Depth depth = ctxDepth
                in ctx { ctxDepth = Depth (depth + 1) }
    getDepth = asks ctxDepth
    getVar var = asks (HashMap.lookup var . ctxEnv)
    getVarDepth (EntryRef ref) = entryDepth <$> liftIO (readIORef ref)
    getVarEsc (EntryRef ref) = entryEsc <$> liftIO (readIORef ref)
    markVarEsc (EntryRef ref) = liftIO $ modifyIORef' ref $ \entry ->
        entry { entryEsc = Escaping }

escapeAnalyze :: Expr -> IO Expr
escapeAnalyze expr = runReaderT (runEscapeM (escapeAnalyzeExpr expr)) initCtx
  where initCtx = Context HashMap.empty (Depth 0)

escapeAnalyzeExpr :: MonadEscape m => Expr -> m Expr
escapeAnalyzeExpr = \case
    LVal lv -> LVal <$> escapeAnalyzeLVal lv
    nil@Nil{}    -> pure nil
    int@IntLit{} -> pure int
    str@StrLit{} -> pure str
    Neg e span -> Neg <$> escapeAnalyzeExpr e <*> pure span
    Binop e1 op e2 span ->  Binop
                        <$> escapeAnalyzeExpr e1
                        <*> pure op
                        <*> escapeAnalyzeExpr e2
                        <*> pure span
    Record rec fs span -> do
        fs' <- forM fs $ \f@Field{..} -> do
            fieldValue' <- escapeAnalyzeExpr fieldValue
            pure f { fieldValue = fieldValue' }
        pure $ Record rec fs' span
    Array arr size ini span ->  Array arr
                            <$> escapeAnalyzeExpr size
                            <*> escapeAnalyzeExpr ini
                            <*> pure span
    Assign lv rv span ->  Assign
                      <$> escapeAnalyzeLVal lv
                      <*> escapeAnalyzeExpr rv
                      <*> pure span
    If cond th mel span ->  If
                        <$> escapeAnalyzeExpr cond
                        <*> escapeAnalyzeExpr th
                        <*> sequenceA (escapeAnalyzeExpr <$> mel)
                        <*> pure span
    While cond body span ->  While
                         <$> escapeAnalyzeExpr cond
                         <*> escapeAnalyzeExpr body
                         <*> pure span
    For var _ e1 e2 e3 span -> do
        (refs, (e1', e2', e3')) <- withLet [var] $
            (,,) <$> escapeAnalyzeExpr e1
                 <*> escapeAnalyzeExpr e2
                 <*> escapeAnalyzeExpr e3
        esc <- case refs of
            (r:_) -> getVarEsc r
            _     -> pure Remaining
        pure (For var esc e1' e2' e3' span)
    Seq es span -> Seq <$> mapM escapeAnalyzeExpr es <*> pure span
    Call fn as span -> Call fn <$> mapM escapeAnalyzeExpr as <*> pure span
    b@Break{} -> pure b
    Let ds e letSpan -> do
        let collectVars vs = \case
                VarDec v _ _ _ _ -> v:vs
                _                -> vs
        let vars = reverse $ foldl collectVars [] ds
        (refs, (ds', e')) <- withLet vars $ do
            ds' <- forM ds $ \case
                tDecs@TypeDecs{} -> pure tDecs
                VarDec var typ val esc varSpan -> do
                    val' <- escapeAnalyzeExpr val
                    pure (VarDec var typ val' esc varSpan)
                FunDecs fDecs -> FunDecs <$> mapM escapeAnalyzeFunDec fDecs
            e' <- escapeAnalyzeExpr e
            pure (ds', e')

        let getEscapings (r:rs, resDs) dec = case dec of
                VarDec var typ val _ varSpan -> do
                    esc <- getVarEsc r
                    pure (rs, VarDec var typ val esc varSpan:resDs)
                _ -> pure (r:rs, dec:resDs)
            getEscapings (rs, resDs) dec = pure (rs, dec:resDs)

        ds'' <- NonEmpty.fromList . reverse . snd <$> foldM getEscapings (refs, []) ds'
        pure (Let ds'' e' letSpan)

escapeAnalyzeLVal :: MonadEscape m => LVal -> m LVal
escapeAnalyzeLVal = \case
    Var var span -> do
        getVar var >>= \case
            Just ref -> do
                varDepth <- getVarDepth ref
                curDepth <- getDepth
                when (varDepth /= curDepth) (markVarEsc ref)
            Nothing -> pure ()
        pure (Var var span)
    Dot lv field span -> Dot <$> escapeAnalyzeLVal lv <*> pure field <*> pure span
    Index lv ind span ->  Index
                      <$> escapeAnalyzeLVal lv
                      <*> escapeAnalyzeExpr ind
                      <*> pure span

escapeAnalyzeFunDec :: MonadEscape m => FunDec -> m FunDec
escapeAnalyzeFunDec dec@FunDec{..} = do
    (refs, funBody') <- withFunction (map decFieldName funArgs) $
        escapeAnalyzeExpr funBody
    funArgs' <- forM (zip funArgs refs) $ \(arg, ref) -> do
        esc <- getVarEsc ref
        pure arg { decFieldEscape = esc }
    pure dec { funBody = funBody', funArgs = funArgs' }
