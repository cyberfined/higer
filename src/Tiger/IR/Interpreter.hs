{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Tiger.IR.Interpreter
    ( Emulator(..)
    , FrameEmulator
    , MonadInterpret(..)
    , InterpreterResult(..)
    , Interpretable
    , runInterpreter
    , interpreterErrorBuilder
    ) where

import           Control.Exception           (ErrorCall, Exception, handle, throwIO)
import           Control.Monad               (foldM, forM_, join, void, when)
import           Control.Monad.Error.Class   (MonadError (..))
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Reader        (MonadReader (..), ReaderT (..), asks)
import           Data.Bits                   (FiniteBits, shiftL, shiftR, xor, (.&.),
                                              (.|.))
import           Data.Char                   (chr, ord)
import           Data.HashMap.Strict         (HashMap)
import           Data.IORef                  (IORef, modifyIORef', newIORef, readIORef,
                                              writeIORef)
import           Data.Maybe                  (fromJust, isJust, isNothing)
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text)
import           Data.Text.Lazy.Builder      (Builder)
import           Data.Vector.Hashtables      (Dictionary, PrimMonad (PrimState))
import           Data.Vector.Unboxed         (Unbox)
import           Prelude                     hiding (Word, exp, init)

import           Tiger.Frame                 (Frame (..))
import           Tiger.IR.Interpreter.Memory hiding (readMemory, writeMemory)
import           Tiger.IR.Printer
import           Tiger.IR.Types
import           Tiger.Temp                  (Label, Temp, labelBuilder, tempBuilder)

import qualified Data.Graph.Inductive        as Graph
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as LazyText
import qualified Data.Text.Lazy.Builder      as Builder
import qualified Data.Text.Lazy.Builder.Int  as Builder
import qualified Data.Vector.Hashtables      as HashTable
import qualified Data.Vector.Mutable         as MVec
import qualified Data.Vector.Unboxed.Mutable as UMVec

import qualified Tiger.IR.Interpreter.Memory as Memory
import qualified Tiger.Temp                  as Temp

newtype InterpretM f e a = InterpretM
    { runInterpretM :: ReaderT (Context InterpretM f e) IO a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadIO
                       , MonadReader (Context InterpretM f e)
                       )

type HashTable k v = Dictionary (PrimState IO) MVec.MVector k MVec.MVector v
type UKHashTable k v = Dictionary (PrimState IO) UMVec.MVector k MVec.MVector v

data Context m f e = Context
    { ctxMemory          :: !(Memory (Word e))
    , ctxStack           :: !(Stack (Word e))
    , ctxRegisters       :: !(GrowVector (Word e))
    , ctxOutput          :: !(IORef Builder)
    , ctxInput           :: !Text
    , ctxInputPos        :: !(IORef Int)
    , ctxFramePointer    :: !(IORef Int)
    , ctxReturnPointer   :: !(IORef Int)
    , ctxFunctions       :: !(HashTable Label (Function m f e))
    , ctxCurrentStmt     :: !(IORef Stmt)
    , ctxLabels          :: !(HashTable Label (LabelValue m f e))
    , ctxStrings         :: !(UKHashTable Int Text)
    , ctxCurrentFunction :: !(IORef Label)
    , ctxEmulator        :: !e
    }

data LabelValue m f e
    = LabelPos !(m f e ())
    | LabelAddress !Int

data Function m f e = Function
    { funExpr  :: !(m f e ())
    , funFrame :: !f
    }

data InterpreterError
    = UnitializedRegisterAccess !Stmt !Temp
    | UnallocatedMemoryAccess !Stmt
    | UnallocatedStackAccess !Stmt
    | ReduceUnallocatedStack !Stmt
    | UndefinedFunctionCall !Stmt
    | UndefinedStringAccess !Stmt
    | JumpToUndefinedLabel !Label
    | NoReturnAtFunctionEnd !Label
    | VoidFunctionAssignment !Stmt
    | ArgumentsNumberMismatch !Int !Int !Stmt
    | WrongMoveDestination !Stmt
    | ReadPositionLabelValue !Label
    | JumpToStringLabel !Label
    | EndOfFunction
    | Exit !Int

interpreterErrorText :: InterpreterError -> LazyText.Text
interpreterErrorText = Builder.toLazyText . interpreterErrorBuilder

interpreterErrorBuilder :: InterpreterError -> Builder
interpreterErrorBuilder = \case
    UnitializedRegisterAccess stmt reg -> "Unitialized register " <> tempBuilder reg
                                       <> " in\n" <> stmtBuilder stmt
    UnallocatedMemoryAccess stmt -> "Unallocated memory access in\n" <> stmtBuilder stmt
    UnallocatedStackAccess stmt -> "Unallocated stack access in\n" <> stmtBuilder stmt
    ReduceUnallocatedStack stmt -> "Reduce unnalocated stack in\n" <> stmtBuilder stmt
    UndefinedFunctionCall stmt -> "Undefined function in\n" <> stmtBuilder stmt
    UndefinedStringAccess stmt -> "Undefined string access in\n" <> stmtBuilder stmt
    JumpToUndefinedLabel label -> "Jump to undefined label " <> labelBuilder label
    NoReturnAtFunctionEnd func -> "No return statement in function " <> labelBuilder func
    VoidFunctionAssignment stmt -> "Void function result is assigned in\n"
                                <> stmtBuilder stmt
    ArgumentsNumberMismatch exp act stmt -> "Wrong number of arguments: expecting"
                                         <> Builder.decimal exp
                                         <> ", actual " <> Builder.decimal act
                                         <> " in\n" <> stmtBuilder stmt
    WrongMoveDestination stmt -> "Wrong move destination in\n" <> stmtBuilder stmt
    ReadPositionLabelValue label -> "Read position label value " <> labelBuilder label
    JumpToStringLabel label -> "Jump to string label " <> labelBuilder label
    EndOfFunction -> "End of funnction"
    Exit code -> "Exited with code " <> Builder.decimal code

instance Show InterpreterError where
    show = LazyText.unpack . interpreterErrorText

instance Exception InterpreterError

class Emulator e f where
    type Word e
    type UWord e

    newEmulator :: MonadIO m => Proxy f -> m e
    enterFunction :: (MonadInterpret m f e, MonadIO (m f e))
                  => e
                  -> f
                  -> [Word e]
                  -> m f e ()
    exitFunction :: (MonadInterpret m f e, MonadIO (m f e))
                 => e
                 -> f
                 -> m f e ()

type FrameEmulator f e = ( Frame f
                         , Emulator e f
                         , Unbox (Word e)
                         , FiniteBits (Word e)
                         , Integral (Word e)
                         , Num (Word e)
                         , Integral (UWord e)
                         , Show (Word e) -- TODO: REMOVE
                         )

class Monad (m f e) => MonadInterpret m f e where
    setCurrentStmt  :: FrameEmulator f e => Stmt -> m f e ()
    getCurrentStmt  :: FrameEmulator f e => m f e Stmt
    setRegister     :: FrameEmulator f e => Temp -> Word e -> m f e ()
    getRegister     :: FrameEmulator f e => Temp -> m f e (Word e)
    getString       :: FrameEmulator f e => Address -> m f e Text
    allocateStack   :: FrameEmulator f e => Size -> m f e Address
    reduceStack     :: FrameEmulator f e => Size -> m f e ()
    allocateMemory  :: FrameEmulator f e => Size -> m f e Address
    readMemory      :: FrameEmulator f e => Address -> m f e (Word e)
    writeMemory     :: FrameEmulator f e => Address -> Word e -> m f e ()
    getLabelValue   :: FrameEmulator f e => Label -> m f e Int
    newString       :: FrameEmulator f e => Text -> m f e Address
    callFunction    :: FrameEmulator f e => Label -> [Word e] -> m f e ()
    printString     :: FrameEmulator f e => Text -> m f e ()
    readChar        :: FrameEmulator f e => m f e Text
    getCurrentFunc  :: FrameEmulator f e => m f e Label
    jumpLabel       :: FrameEmulator f e => Label -> m f e a

class Interpretable f e b where
    initFunctions :: FrameEmulator f e
                  => IRData b f
                  -> HashTable Label (Function InterpretM f e)
                  -> HashTable Label (LabelValue InterpretM f e)
                  -> IO ()

instance (Frame f, Emulator e f) => Interpretable f e Stmt where
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

instance (Frame f, Emulator e f) => Interpretable f e ControlFlowGraph where
    initFunctions IRData{..} funcsMap labelsMap = mapM_ insertFunc irFunctions
      where insertFunc IRFunction{..} = do
                let key = frameName irFuncFrame
                let (_, _, startBlock, _) = fromJust
                                          $ fst
                                          $ Graph.match 0
                                          $ cfgGraph irFuncBody
                let func = Function { funExpr  = runCFGInterpreterM startBlock
                                    , funFrame = irFuncFrame
                                    }
                HashTable.insert funcsMap key func
                forM_ (Graph.labNodes $ cfgGraph irFuncBody) $ \(_, block@Block{..}) -> do
                    let labelVal = LabelPos $ runCFGInterpreterM block
                    HashTable.insert labelsMap blockLabel labelVal

instance (Frame f, Emulator e f) => MonadError InterpreterError (InterpretM f e) where
    throwError = liftIO . throwIO
    catchError ma handler = do
        ctx <- ask
        let action = pure <$> runReaderT (runInterpretM ma) ctx
        join $ liftIO $ handle (pure . handler) action


instance (Frame f, Emulator e f) => MonadInterpret InterpretM f e where
    setCurrentStmt stmt = do
        stmtRef <- asks ctxCurrentStmt
        liftIO $ writeIORef stmtRef stmt
    getCurrentStmt = asks ctxCurrentStmt >>= liftIO . readIORef
    setRegister tmp x = case tmp of
        Temp.Temp t -> do
            regs <- asks ctxRegisters
            writeGrowVector regs t x
        Temp.FP -> do
            fpRef <- asks ctxFramePointer
            liftIO $ writeIORef fpRef (fromIntegral x)
        Temp.RV -> do
            rvRef <- asks ctxReturnPointer
            liftIO $ writeIORef rvRef (fromIntegral x)
    getRegister = \case
        tmp@(Temp.Temp t) -> do
            hdl <- getExceptionHandler (`UnitializedRegisterAccess` tmp)
            regs <- asks ctxRegisters
            liftIO $ handle hdl $ readGrowVector regs t
        Temp.FP -> fromIntegral <$> (asks ctxFramePointer >>= liftIO . readIORef)
        Temp.RV -> fromIntegral <$> (asks ctxReturnPointer >>= liftIO . readIORef)
    getString (Address addr) = do
        stringsMap <- asks ctxStrings
        liftIO (HashTable.lookup stringsMap addr) >>= \case
            Nothing  -> throwInterpretException UndefinedStringAccess
            Just str -> pure str
    allocateStack size = do
        stack <- asks ctxStack
        Memory.allocStack stack size
    reduceStack size = do
        stack <- asks ctxStack
        hdl <- getExceptionHandler ReduceUnallocatedStack
        liftIO $ handle hdl $ Memory.reduceStack stack size
    allocateMemory size = do
        mem <- asks ctxMemory
        Memory.allocMemory mem size
    readMemory addr = do
        inStack <- isAddrInStack addr
        (readMem, exc) <- asks $
            if inStack
            then (,UnallocatedStackAccess) . Memory.readStack . ctxStack
            else (,UnallocatedMemoryAccess) . Memory.readMemory . ctxMemory
        hdl <- getExceptionHandler exc
        liftIO $ handle hdl $ readMem addr
    writeMemory addr x = do
        inStack <- isAddrInStack addr
        (writeMem, exc) <- asks $
            if inStack
            then (,UnallocatedStackAccess) . Memory.writeStack . ctxStack
            else (,UnallocatedMemoryAccess) . Memory.writeMemory . ctxMemory
        hdl <- getExceptionHandler exc
        liftIO $ handle hdl $ writeMem addr x
    getLabelValue l = do
        labelsMap <- asks ctxLabels
        liftIO (HashTable.lookup labelsMap l) >>= \case
            Just val -> case val of
                LabelAddress addr -> pure addr
                LabelPos{}        -> liftIO $ throwIO $ ReadPositionLabelValue l
            Nothing -> liftIO $ throwIO $ JumpToUndefinedLabel l
    newString str = do
        addr@(Address key) <- allocateMemory (Size (wordSize (Proxy @f)))
        stringsMap <- asks ctxStrings
        liftIO $ HashTable.insert stringsMap key str
        pure addr
    callFunction funLabel args = do
        funcsMap <- asks ctxFunctions
        liftIO (HashTable.lookup funcsMap funLabel) >>= \case
            Nothing   -> throwInterpretException UndefinedFunctionCall
            Just Function{..} -> do
                currentFuncRef <- asks ctxCurrentFunction
                prevFunc <- liftIO $ readIORef currentFuncRef
                liftIO $ writeIORef currentFuncRef funLabel
                emu <- asks ctxEmulator
                enterFunction emu funFrame args
                let eofHandler = \case
                        EndOfFunction -> pure ()
                        e             -> throwError e
                catchError funExpr eofHandler
                exitFunction emu funFrame
                liftIO $ writeIORef currentFuncRef prevFunc
    printString str = do
        outputRef <- asks ctxOutput
        liftIO $ modifyIORef' outputRef (<> Builder.fromText str)
    readChar = do
        inputPosRef <- asks ctxInputPos
        input <- asks ctxInput
        pos <- liftIO $ readIORef inputPosRef
        if Text.length input > pos
        then liftIO $ do
            modifyIORef' inputPosRef (+1)
            pure $ Text.singleton $ Text.index input pos
        else pure ""
    getCurrentFunc = asks ctxCurrentFunction >>= liftIO . readIORef
    jumpLabel l = do
        labelsMap <- asks ctxLabels
        liftIO (HashTable.lookup labelsMap l) >>= \case
            Just val -> case val of
                LabelPos pos   -> pos >> throwError EndOfFunction
                LabelAddress{} -> liftIO $ throwIO $ JumpToStringLabel l
            Nothing -> liftIO $ throwIO $ JumpToUndefinedLabel l

data InterpreterResult = InterpreterResult
    { resOutput :: !LazyText.Text
    , resCode   :: !Int
    , resError  :: !(Maybe InterpreterError)
    }

runInterpreter :: forall f e b. (FrameEmulator f e, Interpretable f e b)
               => e
               -> IRData b f
               -> Text
               -> IO InterpreterResult
runInterpreter emu ir@IRData{..} input = do
    memory <- Memory.newMemory @(Word e) (Size 64) (Address memBaseAddr)
    stack <- Memory.newStack @(Word e) (Size 64) (Address stBaseAddr)
    regs <- Memory.newGrowVector @(Word e) (Size 64)
    outputRef <- newIORef mempty
    fpRef <- newIORef stBaseAddr
    rvRef <- newIORef 0
    stmtRef <- newIORef undefined
    currentFuncRef <- newIORef mainLabel
    inputPosRef <- newIORef 0

    stringsMap <- HashTable.initialize 16
    labelsMap <- HashTable.initialize 64
    let insertString LabeledString{..} = do
            Address addr <- Memory.allocMemory memory (Size (wordSize (Proxy @f)))
            HashTable.insert stringsMap addr lStringValue
            HashTable.insert labelsMap lStringLabel (LabelAddress addr)
    mapM_ insertString irStrings

    funcsMap <- HashTable.initialize 16
    initFunctions ir funcsMap labelsMap

    let ctx = Context { ctxMemory          = memory
                      , ctxStack           = stack
                      , ctxRegisters       = regs
                      , ctxOutput          = outputRef
                      , ctxInput           = input
                      , ctxInputPos        = inputPosRef
                      , ctxFramePointer    = fpRef
                      , ctxReturnPointer   = rvRef
                      , ctxFunctions       = funcsMap
                      , ctxLabels          = labelsMap
                      , ctxStrings         = stringsMap
                      , ctxCurrentStmt     = stmtRef
                      , ctxCurrentFunction = currentFuncRef
                      , ctxEmulator        = emu
                      }

    let callMain = void $ callFunction mainLabel []
    let result code err = do
            output <- readIORef outputRef
            pure $ InterpreterResult { resOutput = Builder.toLazyText output
                                     , resCode   = code
                                     , resError  = err
                                     }
    let callMainIO = runReaderT (runInterpretM callMain) ctx
    handle errorHandler (Right <$> callMainIO) >>= \case
        Left (Exit code) -> result code Nothing
        Left err         -> result 1 (Just err)
        Right ()         -> result 0 Nothing
  where stBaseAddr = 0x7ff8
        memBaseAddr = 0x8000
        mainLabel = Temp.LabelText "main"

        errorHandler :: InterpreterError -> IO (Either InterpreterError a)
        errorHandler = pure . Left

runStmtInterpreterM :: ( FrameEmulator f e
                       , MonadError InterpreterError (m f e)
                       , MonadInterpret m f e
                       )
                => Maybe Label
                -> Stmt
                -> m f e ()
runStmtInterpreterM skipLblInit = void . runStmtM skipLblInit

runCFGInterpreterM :: ( FrameEmulator f e
                      , MonadError InterpreterError (m f e)
                      , MonadInterpret m f e
                      )
                   => Block
                   -> m f e ()
runCFGInterpreterM Block{..} = do
    mapM_ (runStmtM Nothing) blockStmts
    case blockNeighs of
        OneNeigh n1 -> jumpLabel n1
        _           -> error $ "block should have one neigh"

runStmtM :: forall m f e. ( FrameEmulator f e
                          , MonadError InterpreterError (m f e)
                          , MonadInterpret m f e
                          )
         => Maybe Label
         -> Stmt
         -> m f e (Maybe Label)
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
  where runExprM :: Maybe Label -> Expr -> m f e (Maybe Label, Word e)
        runExprM skipLbl = \case
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
                let regularCall = callFunction funLabel argVals
                when (isNothing skipLbl') $
                    case funLabel of
                        Temp.LabelText funName
                          | Just libFunc <- HashMap.lookup funName libFuncs
                          -> libFunc argVals
                          | otherwise
                          -> regularCall
                        _ -> regularCall

                res <- if isNothing skipLbl'
                       then getRegister Temp.RV
                       else pure 0

                pure (skipLbl', res)
            ESeq s e -> do
                skipLbl' <- runStmtM skipLbl s
                runExprM skipLbl' e

        move :: Maybe Label -> Expr -> Expr -> m f e (Maybe Label)
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

        calcOperands :: Maybe Label
                     -> Expr
                     -> Expr
                     -> m f e (Maybe Label, Word e, Word e)
        calcOperands skipLbl e1 e2 = do
            (skipLbl', v1) <- runExprM skipLbl e1
            (skipLbl'', v2) <- runExprM skipLbl' e2
            pure (skipLbl'', v1, v2)


        binop :: Binop -> Word e -> Word e -> Word e
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

        relop :: Relop -> Word e -> Word e -> Bool
        relop = \case
            Eq  -> (==)
            Ne  -> (/=)
            Lt  -> (<)
            Le  -> (<=)
            Gt  -> (>)
            Ge  -> (>=)


type LibFunc m f e = [Word e] -> m f e ()

libFuncs :: forall m f e. (FrameEmulator f e
                          , MonadError InterpreterError (m f e)
                          , MonadInterpret m f e
                          )
         => HashMap Text (LibFunc m f e)
libFuncs = HashMap.fromList funcsList
  where funcsList = [ ("print", printFunc)
                    , ("flush", flushFunc)
                    , ("getchar", getCharFunc)
                    , ("ord", ordFunc)
                    , ("chr", chrFunc)
                    , ("size", sizeFunc)
                    , ("substring", substringFunc)
                    , ("concat", concatFunc)
                    , ("not", notFunc)
                    , ("exit", exitFunc)
                    , ("createArray", createArrayFunc)
                    , ("stringEqual", stringEqualFunc)
                    , ("createRecord", createRecordFunc)
                    ]

        printFunc :: LibFunc m f e
        printFunc [str] = getString (Address $ fromIntegral str) >>= printString
        printFunc args  = argumentsNumMismatch 1 (length args)

        flushFunc :: LibFunc m f e
        flushFunc []   = pure ()
        flushFunc args = argumentsNumMismatch 0 (length args)

        getCharFunc :: LibFunc m f e
        getCharFunc [] = do
            str <- readChar
            Address addr <- newString str
            setRegister Temp.RV $ fromIntegral addr
        getCharFunc args = argumentsNumMismatch 0 (length args)

        ordFunc :: LibFunc m f e
        ordFunc [addr] = do
            str <- getString (Address $ fromIntegral addr)
            let res = if Text.null str
                      then (-1)
                      else fromIntegral $ ord $ Text.head str
            setRegister Temp.RV res
        ordFunc args = argumentsNumMismatch 1 (length args)

        chrFunc :: LibFunc m f e
        chrFunc [code] = do
            let char = chr (fromIntegral code)
            Address addr <- newString (Text.singleton char)
            setRegister Temp.RV $ fromIntegral addr
        chrFunc args = argumentsNumMismatch 1 (length args)

        sizeFunc :: LibFunc m f e
        sizeFunc [addr] = do
            str <- getString (Address $ fromIntegral addr)
            setRegister Temp.RV $ fromIntegral $ Text.length str
        sizeFunc args = argumentsNumMismatch 1 (length args)

        substringFunc :: LibFunc m f e
        substringFunc [addr, from, size] = do
            str <- getString (Address $ fromIntegral addr)
            let len = Text.length str
            let substr = if len > fromInt
                         then Text.take sizeInt $ Text.drop fromInt str
                         else ""
            Address substrAddr <- newString substr
            setRegister Temp.RV $ fromIntegral substrAddr
          where fromInt = fromIntegral from
                sizeInt = fromIntegral size
        substringFunc args = argumentsNumMismatch 3 (length args)

        concatFunc :: LibFunc m f e
        concatFunc [addr1, addr2] = do
            str1 <- getString (Address $ fromIntegral addr1)
            str2 <- getString (Address $ fromIntegral addr2)
            Address concatAddr <- newString $ str1 <> str2
            setRegister Temp.RV $ fromIntegral concatAddr
        concatFunc args = argumentsNumMismatch 2 (length args)

        notFunc :: LibFunc m f e
        notFunc [arg] = setRegister Temp.RV $ if arg == 0 then 1 else 0
        notFunc args  = argumentsNumMismatch 1 (length args)

        exitFunc :: LibFunc m f e
        exitFunc [code] = throwError $ Exit $ fromIntegral code
        exitFunc args   = argumentsNumMismatch 1 (length args)

        createArrayFunc :: LibFunc m f e
        createArrayFunc [size, init] = do
            Address addr <- allocateMemory (Size sizeInt)
            forM_ [addr, addr + ws .. addr + sizeInt - ws] $ \curAddr ->
                writeMemory (Address curAddr) init
            setRegister Temp.RV $ fromIntegral addr
          where sizeInt = fromIntegral size * ws
                ws = wordSize (Proxy @f)
        createArrayFunc args = argumentsNumMismatch 2 (length args)

        createRecordFunc :: LibFunc m f e
        createRecordFunc [size] = createArrayFunc [size, 0]
        createRecordFunc args   = argumentsNumMismatch 1 (length args)

        stringEqualFunc :: LibFunc m f e
        stringEqualFunc [addr1, addr2] = do
            str1 <- getString (Address $ fromIntegral addr1)
            str2 <- getString (Address $ fromIntegral addr2)
            setRegister Temp.RV $ if str1 == str2 then 1 else 0
        stringEqualFunc args = argumentsNumMismatch 2 (length args)

        argumentsNumMismatch :: Int -> Int -> m f e a
        argumentsNumMismatch exp act = do
            stmt <- getCurrentStmt
            throwError $ ArgumentsNumberMismatch exp act stmt

withCurrentStmt :: (FrameEmulator f e, MonadInterpret m f e)
                => Stmt
                -> m f e a
                -> m f e a
withCurrentStmt stmt f = do
    curStmt <- getCurrentStmt
    setCurrentStmt stmt
    res <- f
    setCurrentStmt curStmt
    pure res

isAddrInStack :: Address -> InterpretM f e Bool
isAddrInStack addr = do
    stack <- asks ctxStack
    pure $ stackBaseAddr stack >= addr

getExceptionHandler :: (Stmt -> InterpreterError)
                    -> InterpretM f e (ErrorCall -> IO b)
getExceptionHandler cons = do
    stmtRef <- asks ctxCurrentStmt
    pure $ hdl stmtRef
  where hdl stmtRef _ = readIORef stmtRef >>= throwIO . cons

throwInterpretException :: (FrameEmulator f e
                           , MonadError InterpreterError (m f e)
                           , MonadInterpret m f e
                           )
                        => (Stmt -> InterpreterError)
                        -> m f e a
throwInterpretException cons = getCurrentStmt >>= throwError . cons
