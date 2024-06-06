{-# LANGUAGE ScopedTypeVariables #-}

module Tiger.Translate
    ( MonadTranslate(..)
    , Level
    , Access
    , withMainFrame
    , withNewFrame
    , allocLocal
    , irNil
    , irInt
    , irString
    , transNeg
    , transVar
    , transDot
    , transIndex
    , transBinop
    , transRecord
    , transArray
    , transAssign
    , transIf
    , transIfElse
    , transWhile
    , transFor
    , transBreak
    , transSeq
    , transCall
    , transLet
    , transVarDec
    , newFuncLabel
    ) where

import           Control.Monad              (void)
import           Data.HashSet               (HashSet)
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)

import           Tiger.Expr                 (Escaping (..))
import           Tiger.Frame                (Frame)
import           Tiger.IR.Types
import           Tiger.Semant.LibFunctions  (LibFunction (..))
import           Tiger.Semant.Type
import           Tiger.Temp                 (Label (..), MonadTemp (..), Temp (FP, RV))
import           Tiger.Unique

import qualified Data.HashSet               as HashSet
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

import qualified Tiger.Expr                 as Expr
import qualified Tiger.Frame                as Frame
import qualified Tiger.Semant.LibFunctions  as LibFunctions

class (Frame f, MonadTemp (m f), MonadUnique (m f)) => MonadTranslate m f where
    getCurrentLevel  :: m f (Level f)
    setCurrentLevel  :: Level f -> m f ()
    insertString     :: Text -> Label -> m f ()
    lookupString     :: Text -> m f (Maybe Label)
    insertIRFunction :: IRFunction f -> m f ()

data Level f = Level
    { levelUnique :: !Unique
    , levelParent :: !(Maybe (Level f))
    , levelFrame  :: !f
    }

instance Frame f => Eq (Level f) where
    l1 == l2 = levelUnique l1 == levelUnique l2

data Access f = Access !(Level f) !Frame.Access

withMainFrame :: MonadTranslate m f => m f (Type, IR) -> m f ()
withMainFrame f = void $ withNewFrame' Nothing (LabelText "main") [] (const f)

withNewFrame :: MonadTranslate m f
             => Label
             -> [Escaping]
             -> ([Access f] -> m f (Type, IR))
             -> m f (Type, IR)
withNewFrame funLabel args f = do
    curLevel <- getCurrentLevel
    res <- withNewFrame' (Just curLevel) funLabel (Escaping:args) f
    setCurrentLevel curLevel
    pure res

withNewFrame' :: forall m f. MonadTranslate m f
              => Maybe (Level f)
              -> Label
              -> [Escaping]
              -> ([Access f] -> m f (Type, IR))
              -> m f (Type, IR)
withNewFrame' parentLevel funLabel args f = do
    frame <- Frame.newFrame @f funLabel args
    newLevelUniq <- newUnique
    let newLevel = Level newLevelUniq parentLevel frame
    setCurrentLevel newLevel
    res@(retTyp, bodyIR) <- f $ map (Access newLevel) (tail $ Frame.frameArgs frame)
    bodyStmt <- if retTyp == TUnit
                then irToStmt bodyIR
                else Move (Temp RV) <$> irToExpr bodyIR
    frame' <- levelFrame <$> getCurrentLevel
    let func = IRFunction
            { irFuncBody  = Frame.procEntryExit1 frame bodyStmt
            , irFuncFrame = frame'
            }
    insertIRFunction func
    pure res

allocLocal :: MonadTranslate m f => Escaping -> m f (Access f)
allocLocal esc = do
    level@Level{..} <- getCurrentLevel
    (frame', access) <- Frame.allocLocal levelFrame esc
    let level' = level { levelFrame = frame' }
    setCurrentLevel level'
    pure $ Access level' access

irNil :: IR
irNil = Ex (Const 0)

irInt :: Int -> IR
irInt = Ex . Const

irString :: MonadTranslate m f => Text -> m f IR
irString str = do
    label <- lookupString str >>= \case
        Just l  -> pure l
        Nothing -> newLabel >>= \l -> insertString str l >> pure l
    pure $ Ex $ Name label

transNeg :: MonadTranslate m f => IR -> m f IR
transNeg = fmap (Ex . Binop Sub (Const 0)) . irToExpr

transVar :: MonadTranslate m f => Access f -> m f IR
transVar = fmap Ex . transVarExpr

transVarExpr :: MonadTranslate m f => Access f -> m f Expr
transVarExpr access@(Access _ frAccess)
  | isInMem   = Mem <$> accessToIR access
  | otherwise = accessToIR access
  where isInMem = case frAccess of
            Frame.InReg{}   -> False
            Frame.InFrame{} -> True

transDot :: forall m f. MonadTranslate m f => IR -> Int -> m f IR
transDot recPtr idx = do
    recPtrExpr <- irToExpr recPtr
    let addrExpr = if idx == 0 then recPtrExpr else Binop Add recPtrExpr offset
    pure $ Ex $ Mem addrExpr
  where offset = Const $ idx * Frame.wordSize (Proxy @f)

transIndex :: forall m f. MonadTranslate m f => IR -> IR -> m f IR
transIndex arrPtrIR idxIR = do
    arrPtrExpr <- irToExpr arrPtrIR
    idxExpr <- irToExpr idxIR
    let mulIdx = Binop Mul idxExpr (Const $ Frame.wordSize (Proxy @f))
    pure $ Ex $ Mem $ Binop Add arrPtrExpr mulIdx

transBinop :: forall m f. MonadTranslate m f
           => Expr.Binop
           -> Type
           -> IR
           -> IR
           -> m f IR
transBinop exprOp typ ir1 ir2 = case exprOp of
    Expr.Add -> transArith Add
    Expr.Sub -> transArith Sub
    Expr.Mul -> transArith Mul
    Expr.Div -> transArith Div
    Expr.Lt  -> transRelop Lt
    Expr.Le  -> transRelop Le
    Expr.Gt  -> transRelop Gt
    Expr.Ge  -> transRelop Ge
    Expr.Ne  -> transRelop Ne
    Expr.Eq  -> transRelop Eq
    Expr.And -> do
        tLab1 <- newLabel
        pure $ Cx $ \tLab2 fLab ->
            let stmts = irToCond ir1 tLab1 fLab :| [Label tLab1, irToCond ir2 tLab2 fLab]
            in Seq stmts
    Expr.Or -> do
        fLab1 <- newLabel
        pure $ Cx $ \tLab fLab2 ->
            let stmts = irToCond ir1 tLab fLab1 :| [Label fLab1, irToCond ir2 tLab fLab2]
            in Seq stmts
  where transArith :: MonadTranslate m f => Binop -> m f IR
        transArith op = Ex <$> (Binop op <$> irToExpr ir1 <*> irToExpr ir2)

        transRelop :: MonadTranslate m f
                   => Relop
                   -> m f IR
        transRelop op = do
            e1 <- irToExpr ir1
            e2 <- irToExpr ir2
            pure $ case op of
                Ne -> transEq e1 e2
                Eq -> transEq e1 e2
                _  -> Cx $ \tLab fLab -> CJump op e1 e2 tLab fLab
          where transEq e1 e2
                    | typ /= TString = Cx $ \tLab fLab -> CJump op e1 e2 tLab fLab
                    | otherwise =
                        let extCall = Frame.externalCall (Proxy @f) "stringEqual" [e1, e2]
                        in Cx $ \tLab fLab -> CJump op extCall (Const 0) fLab tLab

transRecord :: forall m f. MonadTranslate m f => [(IR, Int)] -> m f IR
transRecord values = do
    recPtr <- Temp <$> newTemp
    let stmt = Move recPtr createRecord
    initFields <- mapM (toAssign recPtr) values
    pure $ Ex $ ESeq (Seq (stmt :| initFields)) recPtr
  where createRecord = Frame.externalCall (Proxy @f) "createRecord" [numFields]
        numFields = Const $ length values
        toAssign recPtr (value, idx) =
            let offset = Const $ idx * Frame.wordSize (Proxy @f)
                addrExpr = if idx == 0 then recPtr  else Binop Add recPtr offset
            in Move (Mem addrExpr) <$> irToExpr value

transArray :: forall m f. MonadTranslate m f => IR -> IR -> m f IR
transArray sizeIR initIR = do
    sizeExpr <- irToExpr sizeIR
    initExpr <- irToExpr initIR
    pure $ Ex $ Frame.externalCall (Proxy @f) "createArray" [sizeExpr, initExpr]

transAssign :: MonadTranslate m f => IR -> IR -> m f IR
transAssign lvalIR rvalIR = do
    lvalExpr <- irToExpr lvalIR
    rvalExpr <- irToExpr rvalIR
    pure $ Nx $ Move lvalExpr rvalExpr

transIf :: MonadTranslate m f => IR -> IR -> m f IR
transIf condIR thIR = do
    tLab <- newLabel
    fLab <- newLabel
    thStmt <- irToStmt thIR
    pure $ Nx $ Seq $ cond tLab fLab :| [Label tLab, thStmt, Label fLab]
  where cond = irToCond condIR

transIfElse :: MonadTranslate m f => Type -> IR -> IR -> IR -> m f IR
transIfElse typ condIR thIR elIR = do
    tLab <- newLabel
    fLab <- newLabel
    skipLab <- newLabel
    if typ == TUnit
       then do
        thStmt <- irToStmt thIR
        elStmt <- irToStmt elIR
        pure $ Nx $ Seq $ cond tLab fLab :| [ Label tLab
                                            , thStmt
                                            , Jump skipLab
                                            , Label fLab
                                            , elStmt
                                            , Label skipLab
                                            ]
       else do
        resTmp <- Temp <$> newTemp
        thExpr <- irToExpr thIR
        elExpr <- irToExpr elIR
        let seqStmt = Seq $ Move resTmp (Const 0) :| [ cond tLab fLab
                                                     , Label tLab
                                                     , Move resTmp thExpr
                                                     , Jump skipLab
                                                     , Label fLab
                                                     , Move resTmp elExpr
                                                     , Label skipLab
                                                     ]
        pure $ Ex $ ESeq seqStmt resTmp
  where cond = irToCond condIR

transWhile :: MonadTranslate m f => IR -> IR -> Label -> m f IR
transWhile condIR bodyIR fLab = do
    (begLab, tLab) <- (,) <$> newLabel <*> newLabel
    bodyStmt <- irToStmt bodyIR
    pure $ Nx $ Seq $ Label begLab :| [ cond tLab fLab
                                      , Label tLab
                                      , bodyStmt
                                      , Jump begLab
                                      , Label fLab
                                      ]
  where cond = irToCond condIR

transFor :: MonadTranslate m f => Access f -> IR -> IR -> IR -> Label -> m f IR
transFor access startIR endIR bodyIR fLab = do
    (begLab, tLab) <- (,) <$> newLabel <*> newLabel
    endTmp <- Temp <$> newTemp
    varExpr <- transVarExpr access
    startExpr <- irToExpr startIR
    endExpr <- irToExpr endIR
    bodyStmt <- irToStmt bodyIR
    pure $ Nx $ Seq $ Move varExpr startExpr :|
        [ Move endTmp endExpr
        , Label begLab
        , CJump Le varExpr endTmp tLab fLab
        , Label tLab
        , bodyStmt
        , Move varExpr (Binop Add varExpr (Const 1))
        , Jump begLab
        , Label fLab
        ]

transBreak :: Label -> IR
transBreak = Nx . Jump

transSeq :: MonadTranslate m f => Type -> [IR] -> m f IR
transSeq resTyp irs
  | resTyp == TUnit = case NonEmpty.nonEmpty irs of
        Nothing    -> pure irNil
        Just irsNe -> Nx . stripSeq <$> mapM irToStmt irsNe
  | otherwise = case unsnoc irs of
        Nothing      -> error "Empty Seq"
        Just (xs, x) -> case NonEmpty.nonEmpty xs of
            Nothing   -> pure x
            Just xsNe -> do
                seqStmt <- stripSeq <$> mapM irToStmt xsNe
                Ex . ESeq seqStmt <$> irToExpr x

transCall :: forall m f. MonadTranslate m f => Text -> Label -> Level f -> [IR] -> m f IR
transCall funName funLabel parentLevel argsIR = do
    argsExpr <- mapM irToExpr argsIR
    curLevel@Level{..} <- getCurrentLevel
    if HashSet.member funName libFunctions
       then pure $ Ex $ Frame.externalCall (Proxy @f) funName argsExpr
       else do
        let fp = Temp FP
        stLink <- if Frame.frameName levelFrame == funLabel
                  then getStaticLink levelFrame fp
                  else followStaticLinks fp parentLevel curLevel
        pure $ Ex $ Call funLabel (stLink : argsExpr)

libFunctions :: HashSet Text
libFunctions = HashSet.fromList $ map libFunName LibFunctions.libFunctions

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = unsnoc' []
  where unsnoc' acc (x:xs@(_:_)) = unsnoc' (x:acc) xs
        unsnoc' acc [x]          = Just (reverse acc, x)
        unsnoc' _ _              = Nothing

transLet :: MonadTranslate m f => [IR] -> IR -> m f IR
transLet mIRStmts irRes = case NonEmpty.nonEmpty mIRStmts of
    Nothing -> pure irRes
    Just irStmts -> do
        stmts <- mapM irToStmt irStmts
        res <- irToExpr irRes
        pure $ Ex $ ESeq (stripSeq stmts) res

transVarDec :: MonadTranslate m f => IR -> Access f -> m f IR
transVarDec srcIR access = do
    dst <- transVarExpr access
    src <- irToExpr srcIR
    pure $ Nx $ Move dst src

newFuncLabel :: MonadTemp m => Text -> m Label
newFuncLabel funName = newLabel >>= \case
    LabelInt l  -> pure
                 $ LabelText
                 $ LazyText.toStrict
                 $ Builder.toLazyText
                 $ Builder.fromText funName <> "_" <> Builder.decimal l
    LabelText{} -> error "Never executed"

accessToIR :: MonadTranslate m f => Access f -> m f Expr
accessToIR (Access level@Level{..} access) = do
    curLevel <- getCurrentLevel
    fp <- followStaticLinks (Temp FP) level curLevel
    Frame.accessToIR levelFrame access fp

followStaticLinks :: MonadTranslate m f
                  => Expr
                  -> Level f
                  -> Level f
                  -> m f Expr
followStaticLinks fp level innerLevel@Level{..}
  | innerLevel == level = pure fp
  | otherwise = do
      stLink <- getStaticLink levelFrame fp
      case levelParent of
          Nothing          -> error "level does not have a parent"
          Just parentLevel -> followStaticLinks stLink level parentLevel

getStaticLink :: MonadTranslate m f => f -> Expr -> m f Expr
getStaticLink frame frameAddress = do
    op <- Frame.accessToIR frame stLinkAccess frameAddress
    pure $ Mem op
  where stLinkAccess = head $ Frame.frameArgs frame

stripSeq :: NonEmpty Stmt -> Stmt
stripSeq (stmt :| []) = stmt
stripSeq stmts        = Seq stmts

irToExpr :: MonadTemp m => IR -> m Expr
irToExpr = \case
    Ex e       -> pure e
    Nx s       -> pure $ ESeq s (Const 0)
    Cx genStmt -> do
        r <- newTemp
        t <- newLabel
        f <- newLabel
        let stmt = Seq $ Move (Temp r) (Const 1) :| [ genStmt t f
                                                    , Label f
                                                    , Move (Temp r) (Const 0)
                                                    , Label t
                                                    ]
        pure $ ESeq stmt (Temp r)

irToStmt :: MonadTemp m => IR -> m Stmt
irToStmt = \case
    Ex e       -> pure $ Expr e
    Nx s       -> pure s
    Cx genStmt -> do
        tf <- newLabel
        pure $ Seq $ genStmt tf tf :| [Label tf]

irToCond :: IR -> Cond
irToCond ir t f = case ir of
    Ex e -> CJump Ne e (Const 0) t f
    Cx c -> c t f
    Nx{} -> error "Can't translate Stmt to Cond"
