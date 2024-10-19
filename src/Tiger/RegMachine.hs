{-# LANGUAGE FunctionalDependencies #-}

module Tiger.RegMachine
    ( InterpretM
    , HashTable
    , UKHashTable
    , ReturnRegister(..)
    , FrameRegister(..)
    , LabelValue(..)
    , Function(..)
    , InterpreterError(..)
    , Emulator(..)
    , FrameEmulator
    , MonadInterpret(..)
    , Trans(..)
    , Interpretable(..)
    , initFunctionsCFG
    , InterpreterResult(..)
    , runInterpreter
    , libFuncs
    , withCurrentStmt
    , throwInterpretException
    ) where

import           Control.Exception           (ErrorCall, Exception, handle, throwIO)
import           Control.Monad               (forM_, join, void)
import           Control.Monad.Error.Class   (MonadError (..))
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Reader        (MonadReader (..), ReaderT (..), asks)
import           Control.Monad.Trans.Class   (MonadTrans (..))
import           Data.Bifunctor              (Bifunctor (..))
import           Data.Bits                   (FiniteBits)
import           Data.Char                   (chr, ord)
import           Data.HashMap.Strict         (HashMap)
import           Data.IORef                  (IORef, modifyIORef', newIORef, readIORef,
                                              writeIORef)
import           Data.Kind                   (Type)
import           Data.Maybe                  (fromJust)
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text)
import           Data.Text.Lazy.Builder      (Builder)
import           Data.Typeable               (Typeable)
import           Data.Vector.Hashtables      (Dictionary, PrimMonad (PrimState))
import           Data.Vector.Unboxed         (Unbox)
import           Prelude                     hiding (Word, exp, init)

import           Tiger.Frame                 (Frame (..))
import           Tiger.IR.Types              (Block (..), ControlFlowGraph (..),
                                              IRData (..), IRFunction (..),
                                              LabeledString (..), Neighs (..))
import           Tiger.RegMachine.Memory     hiding (readMemory, reduceStack, writeMemory)
import           Tiger.Temp                  (Label)
import           Tiger.TextUtils             (TextBuildable (..))

import qualified Data.Graph.Inductive        as Graph
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as LazyText
import qualified Data.Text.Lazy.Builder      as Builder
import qualified Data.Text.Lazy.Builder.Int  as Builder
import qualified Data.Vector.Hashtables      as HashTable
import qualified Data.Vector.Mutable         as MVec
import qualified Data.Vector.Unboxed.Mutable as UMVec

import qualified Tiger.RegMachine.Memory     as Memory
import qualified Tiger.Temp                  as Temp

newtype InterpretM f e r s a = InterpretM
    { runInterpretM :: ReaderT (Context InterpretM f e r s) IO a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadIO
                       , MonadReader (Context InterpretM f e r s)
                       )

type HashTable k v = Dictionary (PrimState IO) MVec.MVector k MVec.MVector v

type UKHashTable k v = Dictionary (PrimState IO) UMVec.MVector k MVec.MVector v

newtype ReturnRegister r = ReturnRegister r

newtype FrameRegister r = FrameRegister r

data Context m f e r s = Context
    { ctxMemory          :: !(Memory (Word e))
    , ctxStack           :: !(Stack (Word e))
    , ctxRegisters       :: !(GrowVector (Word e))
    , ctxOutput          :: !(IORef Builder)
    , ctxInput           :: !Text
    , ctxInputPos        :: !(IORef Int)
    , ctxFunctions       :: !(HashTable Label (Function m f e r s))
    , ctxCurrentStmt     :: !(IORef s)
    , ctxLabels          :: !(HashTable Label (LabelValue m f e r s))
    , ctxStrings         :: !(UKHashTable Int Text)
    , ctxCurrentFunction :: !(IORef Label)
    , ctxEmulator        :: !e
    , ctxReturnRegister  :: !r
    , ctxFrameRegister   :: !r
    }

data LabelValue m f e r s
    = LabelPos !(m f e r s ())
    | LabelAddress !Int

data Function m f e r s = Function
    { funExpr  :: !(m f e r s ())
    , funFrame :: !f
    }

data InterpreterError r s
    = UnitializedRegisterAccess !r !s
    | UnallocatedMemoryAccess !s
    | UnallocatedStackAccess !s
    | ReduceUnallocatedStack !s
    | UndefinedFunctionCall !s
    | UndefinedStringAccess !s
    | JumpToUndefinedLabel !Label
    | NoReturnAtFunctionEnd !Label
    | VoidFunctionAssignment !s
    | ArgumentsNumberMismatch !Int !Int !s
    | WrongMoveDestination !s
    | ReadPositionLabelValue !Label
    | JumpToStringLabel !Label
    | EndOfFunction
    | Exit !Int
    deriving Functor

instance Bifunctor InterpreterError where
    bimap f g = \case
        UnitializedRegisterAccess r s -> UnitializedRegisterAccess (f r) (g s)
        UnallocatedMemoryAccess s     -> UnallocatedMemoryAccess (g s)
        UnallocatedStackAccess s      -> UnallocatedStackAccess (g s)
        ReduceUnallocatedStack s      -> ReduceUnallocatedStack (g s)
        UndefinedFunctionCall s       -> UndefinedFunctionCall (g s)
        UndefinedStringAccess s       -> UndefinedStringAccess (g s)
        JumpToUndefinedLabel l        -> JumpToUndefinedLabel l
        NoReturnAtFunctionEnd l       -> NoReturnAtFunctionEnd l
        VoidFunctionAssignment s      -> VoidFunctionAssignment (g s)
        ArgumentsNumberMismatch e a s -> ArgumentsNumberMismatch e a (g s)
        WrongMoveDestination s        -> WrongMoveDestination (g s)
        ReadPositionLabelValue l      -> ReadPositionLabelValue l
        JumpToStringLabel l           -> JumpToStringLabel l
        EndOfFunction                 -> EndOfFunction
        Exit c                        -> Exit c

instance (TextBuildable r, TextBuildable s) => TextBuildable (InterpreterError r s) where
    toTextBuilder = \case
        UnitializedRegisterAccess reg stmt -> "Unitialized register "
                                           <> toTextBuilder reg
                                           <> " in\n" <> toTextBuilder stmt
        UnallocatedMemoryAccess stmt -> "Unallocated memory access in\n"
                                     <> toTextBuilder stmt
        UnallocatedStackAccess stmt -> "Unallocated stack access in\n"
                                    <> toTextBuilder stmt
        ReduceUnallocatedStack stmt -> "Reduce unnalocated stack in\n"
                                    <> toTextBuilder stmt
        UndefinedFunctionCall stmt -> "Undefined function in\n" <> toTextBuilder stmt
        UndefinedStringAccess stmt -> "Undefined string access in\n" <> toTextBuilder stmt
        JumpToUndefinedLabel label -> "Jump to undefined label " <> toTextBuilder label
        NoReturnAtFunctionEnd func -> "No return statement in function "
                                   <> toTextBuilder func
        VoidFunctionAssignment stmt -> "Void function result is assigned in\n"
                                    <> toTextBuilder stmt
        ArgumentsNumberMismatch exp act stmt -> "Wrong number of arguments: expecting"
                                             <> Builder.decimal exp
                                             <> ", actual " <> Builder.decimal act
                                             <> " in\n" <> toTextBuilder stmt
        WrongMoveDestination stmt -> "Wrong move destination in\n" <> toTextBuilder stmt
        ReadPositionLabelValue label -> "Read position label value "
                                     <> toTextBuilder label
        JumpToStringLabel label -> "Jump to string label " <> toTextBuilder label
        EndOfFunction -> "End of funnction"
        Exit code -> "Exited with code " <> Builder.decimal code

instance (TextBuildable r, TextBuildable s) => Show (InterpreterError r s) where
    show = LazyText.unpack . Builder.toLazyText . toTextBuilder

instance (Typeable r, TextBuildable r, Typeable s, TextBuildable s) =>
    Exception (InterpreterError r s)

class Emulator e f r s | e -> r, e -> s where
    type Word e
    type UWord e

    newEmulator :: MonadIO m => Proxy f -> m e
    enterFunction :: (MonadInterpret f e r s m, MonadIO (m f e r s))
                  => e
                  -> f
                  -> [Word e]
                  -> m f e r s ()
    exitFunction :: (MonadInterpret f e r s m, MonadIO (m f e r s))
                 => e
                 -> f
                 -> m f e r s ()

type FrameEmulator f e r s = ( Frame f
                             , Emulator e f r s
                             , Unbox (Word e)
                             , FiniteBits (Word e)
                             , Integral (Word e)
                             , Num (Word e)
                             , Integral (UWord e)
                             )

class (Enum r, Monad (m f e r s)) => MonadInterpret f e r s m where
    setCurrentStmt    :: FrameEmulator f e r s => s -> m f e r s ()
    getCurrentStmt    :: FrameEmulator f e r s => m f e r s s
    setRegister       :: FrameEmulator f e r s => r -> Word e -> m f e r s ()
    getRegister       :: FrameEmulator f e r s => r -> m f e r s (Word e)
    getString         :: FrameEmulator f e r s => Address -> m f e r s Text
    allocateStack     :: FrameEmulator f e r s => Size -> m f e r s Address
    reduceStack       :: FrameEmulator f e r s => Size -> m f e r s ()
    allocateMemory    :: FrameEmulator f e r s => Size -> m f e r s Address
    readMemory        :: FrameEmulator f e r s => Address -> m f e r s (Word e)
    writeMemory       :: FrameEmulator f e r s => Address -> Word e -> m f e r s ()
    getLabelValue     :: FrameEmulator f e r s => Label -> m f e r s Int
    newString         :: FrameEmulator f e r s => Text -> m f e r s Address
    callFunction      :: FrameEmulator f e r s => Label -> [Word e] -> m f e r s ()
    printString       :: FrameEmulator f e r s => Text -> m f e r s ()
    readChar          :: FrameEmulator f e r s => m f e r s Text
    getCurrentFunc    :: FrameEmulator f e r s => m f e r s Label
    jumpLabel         :: FrameEmulator f e r s => Label -> m f e r s a
    getReturnRegister :: FrameEmulator f e r s => m f e r s r
    getFramePointer   :: FrameEmulator f e r s => m f e r s r

newtype Trans (n :: (Type -> Type) -> Type -> Type) m f e r s a = Trans (n (m f e r s) a)
    deriving newtype (Functor, Applicative, Monad)

instance (MonadInterpret f e r s m, MonadTrans n, Monad (n (m f e r s))) =>
    MonadInterpret f e r s (Trans n m) where
    setCurrentStmt = Trans . lift . setCurrentStmt
    getCurrentStmt = Trans $ lift getCurrentStmt
    setRegister reg x = Trans $ lift $ setRegister reg x
    getRegister = Trans . lift . getRegister
    getString = Trans . lift .getString
    allocateStack = Trans . lift . allocateStack
    reduceStack = Trans . lift . reduceStack
    allocateMemory = Trans . lift . allocateMemory
    readMemory = Trans . lift . readMemory
    writeMemory addr x = Trans $ lift $ writeMemory addr x
    getLabelValue = Trans . lift . getLabelValue
    newString = Trans . lift . newString
    callFunction funLabel args = Trans $ lift $ callFunction funLabel args
    printString = Trans . lift . printString
    readChar = Trans $ lift readChar
    getCurrentFunc = Trans $ lift getCurrentFunc
    jumpLabel = Trans . lift . jumpLabel
    getReturnRegister = Trans $ lift getReturnRegister
    getFramePointer = Trans $ lift getFramePointer

class ( Frame f
      , Emulator e f r s
      , Typeable r
      , TextBuildable r
      , Enum r
      , Typeable s
      , TextBuildable s
      ) => Interpretable f e r s b where
    initFunctions :: FrameEmulator f e r s
                  => IRData b f
                  -> HashTable Label (Function InterpretM f e r s)
                  -> HashTable Label (LabelValue InterpretM f e r s)
                  -> IO ()

initFunctionsCFG :: ( MonadInterpret f e r s m
                    , FrameEmulator f e r s
                    , Typeable r
                    , TextBuildable r
                    , Enum r
                    , Typeable s
                    , TextBuildable s
                    )
                 => (s -> m f e r s ())
                 -> IO (m f e r s () -> InterpretM f e r s ())
                 -> IRData (ControlFlowGraph s) f
                 -> HashTable Label (Function InterpretM f e r s)
                 -> HashTable Label (LabelValue InterpretM f e r s)
                 -> IO ()
initFunctionsCFG run getUnlift IRData{..} funcsMap labelsMap = do
    unlift <- getUnlift
    mapM_ (insertFunc unlift) irFunctions
  where runCFGInterpreterM Block{..} = do
            mapM_ run blockStmts
            case blockNeighs of
                OneNeigh n1 -> jumpLabel n1
                _           -> error "block should have one neigh"

        insertFunc unlift IRFunction{..} = do
            let key = frameName irFuncFrame
            let (_, _, startBlock, _) = fromJust
                                      $ fst
                                      $ Graph.match 0
                                      $ cfgGraph irFuncBody
            let func = Function { funExpr  = unlift $ runCFGInterpreterM startBlock
                                , funFrame = irFuncFrame
                                }
            HashTable.insert funcsMap key func
            forM_ (Graph.labNodes $ cfgGraph irFuncBody) $ \(_, block@Block{..}) -> do
                let labelVal = LabelPos $ unlift $ runCFGInterpreterM block
                HashTable.insert labelsMap blockLabel labelVal

instance ( Typeable r
         , TextBuildable r
         , Typeable s
         , TextBuildable s
         , Frame f
         , Emulator e f r s
         ) => MonadError (InterpreterError r s) (InterpretM f e r s) where
    throwError = liftIO . throwIO
    catchError ma handler = do
        ctx <- ask
        let action = pure <$> runReaderT (runInterpretM ma) ctx
        join $ liftIO $ handle (pure . handler) action

instance ( Frame f
         , Typeable r
         , TextBuildable r
         , Enum r
         , Typeable s
         , TextBuildable s
         , Emulator e f r s
         ) => MonadInterpret f e r s InterpretM where
    setCurrentStmt stmt = do
        stmtRef <- asks ctxCurrentStmt
        liftIO $ writeIORef stmtRef stmt
    getCurrentStmt = asks ctxCurrentStmt >>= liftIO . readIORef
    setRegister reg x = do
        regs <- asks ctxRegisters
        writeGrowVector regs (fromEnum reg) x
    getRegister reg = do
        hdl <- getExceptionHandler (reg `UnitializedRegisterAccess`)
        regs <- asks ctxRegisters
        liftIO $ handle hdl $ readGrowVector regs (fromEnum reg)
    getString (Address addr) = do
        stringsMap <- asks ctxStrings
        InterpretM (HashTable.lookup stringsMap addr) >>= \case
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
                LabelPos{}        -> throwError $ ReadPositionLabelValue l
            Nothing -> throwError $ JumpToUndefinedLabel l
    newString str = do
        addr@(Address key) <- allocateMemory (Size (wordSize (Proxy @f)))
        stringsMap <- asks ctxStrings
        InterpretM $ HashTable.insert stringsMap key str
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
                LabelAddress{} -> throwError $ JumpToStringLabel l
            Nothing -> throwError $ JumpToUndefinedLabel l
    getReturnRegister = asks ctxReturnRegister
    getFramePointer = asks ctxFrameRegister

data InterpreterResult r s = InterpreterResult
    { resOutput :: !LazyText.Text
    , resCode   :: !Int
    , resError  :: !(Maybe (InterpreterError r s))
    } deriving Functor

instance Bifunctor InterpreterResult where
    bimap f g res = res { resError = bimap f g <$> resError res }

runInterpreter :: forall f e r s b. ( Typeable r
                                    , TextBuildable r
                                    , Enum r
                                    , Typeable s
                                    , TextBuildable s
                                    , FrameEmulator f e r s
                                    , Interpretable f e r s b
                                    )
               => ReturnRegister r
               -> FrameRegister r
               -> e
               -> IRData b f
               -> Text
               -> IO (InterpreterResult r s)
runInterpreter rv fp emu ir@IRData{..} input = do
    memory <- Memory.newMemory @(Word e) (Size 64) (Address memBaseAddr)
    stack <- Memory.newStack @(Word e) (Size 64) (Address stBaseAddr)
    regs <- Memory.newGrowVector @(Word e) (Size 64)
    outputRef <- liftIO $ newIORef mempty
    stmtRef <- liftIO $ newIORef undefined
    currentFuncRef <- liftIO $ newIORef mainLabel
    inputPosRef <- liftIO $ newIORef 0

    funcsMap <- HashTable.initialize 16
    labelsMap <- HashTable.initialize 64
    initFunctions ir funcsMap labelsMap

    stringsMap <- HashTable.initialize 16
    let insertString LabeledString{..} = do
            Address addr <- Memory.allocMemory memory (Size (wordSize (Proxy @f)))
            HashTable.insert stringsMap addr lStringValue
            HashTable.insert labelsMap lStringLabel (LabelAddress addr)
    mapM_ insertString irStrings

    let ctx = Context { ctxMemory          = memory
                      , ctxStack           = stack
                      , ctxRegisters       = regs
                      , ctxOutput          = outputRef
                      , ctxInput           = input
                      , ctxInputPos        = inputPosRef
                      , ctxFunctions       = funcsMap
                      , ctxLabels          = labelsMap
                      , ctxStrings         = stringsMap
                      , ctxCurrentStmt     = stmtRef
                      , ctxCurrentFunction = currentFuncRef
                      , ctxEmulator        = emu
                      , ctxReturnRegister  = let (ReturnRegister r) = rv in r
                      , ctxFrameRegister   = let (FrameRegister r) = fp in r
                      }

    let result code err = do
            output <- liftIO $ readIORef outputRef
            pure $ InterpreterResult { resOutput = Builder.toLazyText output
                                     , resCode   = code
                                     , resError  = err
                                     }
    handle errorHandler (Right <$> runReaderT (runInterpretM callMain) ctx) >>= \case
        Left (Exit code) -> result code Nothing
        Left err -> result 1 (Just err)
        Right () -> result 0 Nothing
  where stBaseAddr = 0x7ff8
        memBaseAddr = 0x8000
        errorHandler = pure . Left
        callMain = void $ callFunction mainLabel []
        mainLabel = Temp.LabelText "main"

type LibFunc m f e r s = [Word e] -> m f e r s ()

libFuncs :: forall m f e r s. ( FrameEmulator f e r s
                              , MonadError (InterpreterError r s) (m f e r s)
                              , MonadInterpret f e r s m
                              )
         => HashMap Text (LibFunc m f e r s)
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

        printFunc [str] = getString (Address $ fromIntegral str) >>= printString
        printFunc args  = argumentsNumMismatch 1 (length args)

        flushFunc []   = pure ()
        flushFunc args = argumentsNumMismatch 0 (length args)

        getCharFunc [] = do
            str <- readChar
            Address addr <- newString str
            retReg <- getReturnRegister
            setRegister retReg $ fromIntegral addr
        getCharFunc args = argumentsNumMismatch 0 (length args)

        ordFunc [addr] = do
            str <- getString (Address $ fromIntegral addr)
            let res = if Text.null str
                      then (-1)
                      else fromIntegral $ ord $ Text.head str
            retReg <- getReturnRegister
            setRegister retReg res
        ordFunc args = argumentsNumMismatch 1 (length args)

        chrFunc [code] = do
            let char = chr (fromIntegral code)
            Address addr <- newString (Text.singleton char)
            retReg <- getReturnRegister
            setRegister retReg $ fromIntegral addr
        chrFunc args = argumentsNumMismatch 1 (length args)

        sizeFunc [addr] = do
            str <- getString (Address $ fromIntegral addr)
            retReg <- getReturnRegister
            setRegister retReg $ fromIntegral $ Text.length str
        sizeFunc args = argumentsNumMismatch 1 (length args)

        substringFunc [addr, from, size] = do
            str <- getString (Address $ fromIntegral addr)
            let len = Text.length str
            let substr = if len > fromInt
                         then Text.take sizeInt $ Text.drop fromInt str
                         else ""
            Address substrAddr <- newString substr
            retReg <- getReturnRegister
            setRegister retReg $ fromIntegral substrAddr
          where fromInt = fromIntegral from
                sizeInt = fromIntegral size
        substringFunc args = argumentsNumMismatch 3 (length args)

        concatFunc [addr1, addr2] = do
            str1 <- getString (Address $ fromIntegral addr1)
            str2 <- getString (Address $ fromIntegral addr2)
            Address concatAddr <- newString $ str1 <> str2
            retReg <- getReturnRegister
            setRegister retReg $ fromIntegral concatAddr
        concatFunc args = argumentsNumMismatch 2 (length args)

        notFunc [arg] = do
            retReg <- getReturnRegister
            setRegister retReg $ if arg == 0 then 1 else 0
        notFunc args  = argumentsNumMismatch 1 (length args)

        exitFunc [code] = throwError $ Exit $ fromIntegral code
        exitFunc args   = argumentsNumMismatch 1 (length args)

        createArrayFunc [size, init] = do
            Address addr <- allocateMemory (Size sizeInt)
            forM_ [addr, addr + ws .. addr + sizeInt - ws] $ \curAddr ->
                writeMemory (Address curAddr) init
            retReg <- getReturnRegister
            setRegister retReg $ fromIntegral addr
          where sizeInt = fromIntegral size * ws
                ws = wordSize (Proxy @f)
        createArrayFunc args = argumentsNumMismatch 2 (length args)

        createRecordFunc [size] = createArrayFunc [size, 0]
        createRecordFunc args   = argumentsNumMismatch 1 (length args)

        stringEqualFunc [addr1, addr2] = do
            str1 <- getString (Address $ fromIntegral addr1)
            str2 <- getString (Address $ fromIntegral addr2)
            retReg <- getReturnRegister
            setRegister retReg $ if str1 == str2 then 1 else 0
        stringEqualFunc args = argumentsNumMismatch 2 (length args)

        argumentsNumMismatch exp act = do
            stmt <- getCurrentStmt
            throwError $ ArgumentsNumberMismatch exp act stmt

withCurrentStmt :: (FrameEmulator f e r s, MonadInterpret f e r s m)
                => s
                -> m f e r s a
                -> m f e r s a
withCurrentStmt stmt f = do
    curStmt <- getCurrentStmt
    setCurrentStmt stmt
    res <- f
    setCurrentStmt curStmt
    pure res

isAddrInStack :: Address -> InterpretM f e r s Bool
isAddrInStack addr = do
    stack <- asks ctxStack
    pure $ stackBaseAddr stack >= addr

getExceptionHandler :: (Typeable r, TextBuildable r, Typeable s, TextBuildable s)
                    => (s -> InterpreterError r s)
                    -> InterpretM f e r s (ErrorCall -> IO b)
getExceptionHandler cons = do
    stmtRef <- asks ctxCurrentStmt
    pure $ hdl stmtRef
  where hdl stmtRef _ = readIORef stmtRef >>= throwIO . cons

throwInterpretException :: ( FrameEmulator f e r s
                           , MonadError (InterpreterError r s) (m f e r s)
                           , MonadInterpret f e r s m
                           )
                        => (s -> InterpreterError r s)
                        -> m f e r s a
throwInterpretException cons = getCurrentStmt >>= throwError . cons
