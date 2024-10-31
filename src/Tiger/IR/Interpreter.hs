module Tiger.IR.Interpreter () where

import           Control.Monad             (foldM, void, when)
import           Control.Monad.Error.Class (MonadError, throwError)
import           Data.Bits                 (shiftL, shiftR, xor, (.&.), (.|.))
import           Data.Maybe                (isJust, isNothing)

import           Tiger.Frame               (Frame (..))
import           Tiger.IR.Printer ()
import           Tiger.IR.Types
import           Tiger.RegMachine
import           Tiger.Temp                (Label, Temp)

import qualified Data.HashMap.Strict       as HashMap
import qualified Data.Vector.Hashtables    as HashTable

import qualified Tiger.Temp                as Temp

instance (Frame f, Emulator e f Temp Stmt) => Interpretable f e Temp Stmt Stmt where
    initFunctions IRData{..} funcsMap labelsMap = mapM_ insertFunc irFunctions
      where insertFunc IRFunction{..} = do
                let key = frameName irFuncFrame
                let func = Function { funExpr  = runStmtInterpreterM Nothing irFuncBody
                                    , funFrame = irFuncFrame
                                    }
                HashTable.insert funcsMap key func
                insertLabelsStmt irFuncBody irFuncBody

            insertLabelsStmt  outer = \case
                Move dst src      -> insertLabelsExpr outer dst
                                  >> insertLabelsExpr outer src
                Expr e            -> insertLabelsExpr outer e
                Jump{}            -> pure ()
                CJump _ e1 e2 _ _ -> insertLabelsExpr outer e1
                                  >> insertLabelsExpr outer e2
                Seq ss            -> mapM_ (insertLabelsStmt outer) ss
                Label l           -> HashTable.insert labelsMap l
                                  $  LabelPos $ runStmtInterpreterM (Just l) outer
                Ret               -> pure ()

            insertLabelsExpr outer = \case
                Const{}       -> pure ()
                Name{}        -> pure ()
                Temp{}        -> pure ()
                Binop _ e1 e2 -> insertLabelsExpr outer e1
                              >> insertLabelsExpr outer e2
                Mem e         -> insertLabelsExpr outer e
                Call _ args   -> mapM_ (insertLabelsExpr outer) args
                ESeq s e      -> insertLabelsStmt outer s
                              >> insertLabelsExpr outer e

instance (Frame f, Emulator e f Temp Stmt) =>
    Interpretable f e Temp Stmt (ControlFlowGraph Stmt) where
    initFunctions = initFunctionsCFG (void . runStmtM Nothing) (pure id)

runStmtInterpreterM :: ( FrameEmulator f e Temp Stmt
                       , MonadError (InterpreterError Temp Stmt) (m f e Temp Stmt)
                       , MonadInterpret f e Temp Stmt m
                       )
                => Maybe Label
                -> Stmt
                -> m f e Temp Stmt ()
runStmtInterpreterM skipLblInit = void . runStmtM skipLblInit

runStmtM :: forall m f e . ( FrameEmulator f e Temp Stmt
                           , MonadError (InterpreterError Temp Stmt) (m f e Temp Stmt)
                           , MonadInterpret f e Temp Stmt m
                           )
         => Maybe Label
         -> Stmt
         -> m f e Temp Stmt (Maybe Label)
runStmtM skipStmtLbl stmt = withCurrentStmt stmt $ case stmt of
    Move dst src -> move skipStmtLbl dst src
    Expr e -> fst <$> runExprM skipStmtLbl e
    Jump lbl
      | isJust skipStmtLbl -> pure skipStmtLbl
      | otherwise          -> jumpLabel lbl
    CJump op e1 e2 tLab fLab -> do
        (skipLbl', v1, v2) <- calcOperands skipStmtLbl e1 e2
        let nextLab = if relop op v1 v2 then tLab else fLab
        if isJust skipLbl'
           then pure skipLbl'
           else jumpLabel nextLab
    Seq ss -> foldM runStmtM skipStmtLbl ss
    Label l
      | Just skipStmtLbl' <- skipStmtLbl, skipStmtLbl' == l -> pure Nothing
      | otherwise                                           -> pure skipStmtLbl
    Ret -> throwError EndOfFunction
  where runExprM skipLbl = \case
            Const i  -> pure (skipLbl, fromIntegral i)
            Name lbl -> (skipLbl,) . fromIntegral <$> getLabelValue lbl
            Temp t   -> (skipLbl,) <$> getRegister t
            Binop op e1 e2 -> do
                (skipLbl', v1, v2) <- calcOperands skipLbl e1 e2
                let res = if isNothing skipLbl' then binop op v1 v2 else 0
                pure (skipLbl', res)
            Mem e -> do
                (skipLbl', addr) <- fmap (Address . fromIntegral) <$> runExprM skipLbl e
                res <- maybe (readMemory addr) (const $ pure 0) skipLbl'
                pure (skipLbl', res)
            Call funLabel args -> do
                let getArgVals vs (e:es) l = do
                        (l', v) <- runExprM l e
                        getArgVals (v:vs) es l'
                    getArgVals vs _ l = pure (l, reverse vs)
                (skipLbl', argVals) <- getArgVals [] args skipLbl
                when (isNothing skipLbl') $
                    case funLabel of
                        Temp.LabelText funName
                          | Just libFunc <- HashMap.lookup funName libFuncs
                          -> libFunc argVals
                        _ -> callFunction funLabel argVals

                res <- if isNothing skipLbl'
                       then getRegister Temp.RV
                       else pure 0

                pure (skipLbl', res)
            ESeq s e -> do
                skipLbl' <- runStmtM skipLbl s
                runExprM skipLbl' e

        move skipLbl dst src = case dst of
            Temp r -> do
                (skipLbl', srcVal) <- runExprM skipLbl src
                when (isNothing skipLbl') $
                    setRegister r srcVal
                pure skipLbl'
            Mem addr -> do
                (skipLbl', addrVal, srcVal) <- calcOperands skipLbl addr src
                when (isNothing skipLbl') $
                    writeMemory (Address $ fromIntegral addrVal) srcVal
                pure skipLbl'
            ESeq s e -> do
                skipLbl' <- runStmtM skipLbl s
                move skipLbl' e src
            _ -> throwInterpretException WrongMoveDestination

        calcOperands skipLbl e1 e2 = do
            (skipLbl', v1) <- runExprM skipLbl e1
            (skipLbl'', v2) <- runExprM skipLbl' e2
            pure (skipLbl'', v1, v2)

        binop = \case
            Add    -> (+)
            Sub    -> (-)
            Mul    -> (*)
            Div    -> div
            And    -> (.&.)
            Or     -> (.|.)
            Xor    -> xor
            LShift -> \a b -> a `shiftL` fromIntegral b
            RShift -> \a b -> a `shiftR` fromIntegral b

        relop = \case
            Eq  -> (==)
            Ne  -> (/=)
            Lt  -> (<)
            Le  -> (<=)
            Gt  -> (>)
            Ge  -> (>=)
