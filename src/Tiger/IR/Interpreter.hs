{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Tiger.IR.Interpreter
    ( Emulator(..)
    , MonadInterpret(..)
    , InterpreterError(..)
    , interpreterErrorText
    , interpreterErrorBuilder
    , InterpreterResult(..)
    , runInterpreter
    ) where

import           Control.Exception           (ErrorCall, Exception, handle, throwIO)
import           Control.Monad               (forM_, void)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Reader        (MonadReader, ReaderT (runReaderT), asks)
import           Data.Bits                   (FiniteBits, shiftL, shiftR, xor, (.&.),
                                              (.|.))
import           Data.Char                   (chr, ord)
import           Data.HashMap.Strict         (HashMap)
import           Data.IORef                  (IORef, modifyIORef', newIORef, readIORef,
                                              writeIORef)
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text)
import           Data.Text.Lazy.Builder      (Builder)
import           Data.Text.Lazy.Builder.Int  (decimal)
import           Data.Vector                 (Vector)
import           Data.Vector.Hashtables      (Dictionary, PrimMonad (PrimState))
import           Data.Vector.Unboxed         (Unbox)
import           Prelude                     hiding (Word, exp, init)

import           Tiger.Frame                 (Frame (..))
import           Tiger.IR.Interpreter.Memory (Address (..), GrowVector, Memory, Size (..),
                                              Stack, readGrowVector, stackBaseAddr,
                                              writeGrowVector)
import           Tiger.IR.Printer            (irBuilder)
import           Tiger.IR.Types              (Binop (..), IR (..), IRData (..),
                                              IRFunction (..), LabeledString (..),
                                              Operand, Relop (..), Stmt)
import           Tiger.Temp                  (Label, Temp, labelBuilder, tempBuilder)


import qualified Data.HashMap.Strict         as HashMap
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as LazyText
import qualified Data.Text.Lazy.Builder      as Builder
import qualified Data.Vector                 as Vec
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
    , ctxFunctions       :: !(HashTable Label (Function m f e))
    , ctxLabels          :: !(HashTable Label Int)
    , ctxStrings         :: !(UKHashTable Int Text)
    , ctxCurrentStmt     :: !(IORef (IR Stmt))
    , ctxCurrentFunction :: !(IORef Label)
    , ctxEmulator        :: !e
    }

data Function m f e = Function
    { funExpr  :: !(m f e (Maybe (Word e)))
    , funFrame :: !f
    }

data InterpreterError
    = UnitializedRegisterAccess !(IR Stmt) !Temp
    | UnallocatedMemoryAccess !(IR Stmt)
    | UnallocatedStackAccess !(IR Stmt)
    | ReduceUnallocatedStack !(IR Stmt)
    | UndefinedFunctionCall !(IR Stmt)
    | UndefinedStringAccess !(IR Stmt)
    | JumpToUndefinedLabel !Label
    | NoReturnAtFunctionEnd !Label
    | VoidFunctionAssignment !(IR Stmt)
    | ArgumentsNumberMismatch !Int !Int !(IR Stmt)
    | Exit !Int

interpreterErrorText :: InterpreterError -> LazyText.Text
interpreterErrorText = Builder.toLazyText . interpreterErrorBuilder

interpreterErrorBuilder :: InterpreterError -> Builder
interpreterErrorBuilder = \case
    UnitializedRegisterAccess stmt reg -> "Unitialized register " <> tempBuilder reg
                                       <> " in " <> irBuilder stmt
    UnallocatedMemoryAccess stmt -> "Unallocated memory access in " <> irBuilder stmt
    UnallocatedStackAccess stmt -> "Unallocated stack access in " <> irBuilder stmt
    ReduceUnallocatedStack stmt -> "Reduce unnalocated stack in " <> irBuilder stmt
    UndefinedFunctionCall stmt -> "Undefined function in " <> irBuilder stmt
    UndefinedStringAccess stmt -> "Undefined string access in " <> irBuilder stmt
    JumpToUndefinedLabel label -> "Jump to undefined label " <> labelBuilder label
    NoReturnAtFunctionEnd func -> "No return statement in function " <> labelBuilder func
    VoidFunctionAssignment stmt -> "Void function result is assigned in "
                                <> irBuilder stmt
    ArgumentsNumberMismatch exp act stmt -> "Wrong number of arguments: expecting"
                                         <> decimal exp
                                         <> ", actual " <> decimal act
                                         <> " in " <> irBuilder stmt
    Exit code -> "Exited with code " <> decimal code

instance Show InterpreterError where
    show = LazyText.unpack . interpreterErrorText

instance Exception InterpreterError

class Emulator e f where
    type Word e
    type UWord e

    enterFunction :: (MonadInterpret m f e, MonadIO (m f e))
                  => e
                  -> f
                  -> [Word e]
                  -> m f e ()
    exitFunction :: (MonadInterpret m f e, MonadIO (m f e))
                 => e
                 -> f
                 -> m f e ()

class Monad (m f e) => MonadInterpret m f e where
    setCurrentStmt  :: FrameEmulator f e => IR Stmt -> m f e ()
    getCurrentStmt  :: FrameEmulator f e => m f e (IR Stmt)
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
    callFunction    :: FrameEmulator f e => Label -> [Word e] -> m f e (Maybe (Word e))
    printString     :: FrameEmulator f e => Text -> m f e ()
    readChar        :: FrameEmulator f e => m f e Text
    getCurrentFunc  :: FrameEmulator f e => m f e Label
    throwError      :: FrameEmulator f e => InterpreterError -> m f e a

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
    getRegister = \case
        tmp@(Temp.Temp t) -> do
            hdl <- getExceptionHandler (\stmt -> UnitializedRegisterAccess stmt tmp)
            regs <- asks ctxRegisters
            liftIO $ handle hdl $ readGrowVector regs t
        Temp.FP -> fromIntegral <$> (asks ctxFramePointer >>= liftIO . readIORef)
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
        (readMem, exc) <- if inStack
                          then (,UnallocatedStackAccess) <$>
                               (Memory.readStack <$> asks ctxStack)
                          else (,UnallocatedMemoryAccess) <$>
                               (Memory.readMemory <$> asks ctxMemory)
        hdl <- getExceptionHandler exc
        liftIO $ handle hdl $ readMem addr
    writeMemory addr x = do
        inStack <- isAddrInStack addr
        (writeMem, exc) <- if inStack
                           then (,UnallocatedStackAccess) <$>
                                (Memory.writeStack <$> asks ctxStack)
                           else (,UnallocatedMemoryAccess) <$>
                                (Memory.writeMemory <$> asks ctxMemory)
        hdl <- getExceptionHandler exc
        liftIO $ handle hdl $ writeMem addr x
    getLabelValue l = do
        labelsMap <- asks ctxLabels
        liftIO (HashTable.lookup labelsMap l) >>= \case
            Just offset -> pure offset
            Nothing     -> liftIO $ throwIO $ JumpToUndefinedLabel l
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
                res <- funExpr
                exitFunction emu funFrame
                liftIO $ writeIORef currentFuncRef prevFunc
                pure res
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
    throwError = liftIO . throwIO

type FrameEmulator f e = ( Frame f
                         , Emulator e f
                         , Unbox (Word e)
                         , FiniteBits (Word e)
                         , Integral (Word e)
                         , Num (Word e)
                         , Integral (UWord e)
                         )

data InterpreterResult = InterpreterResult
    { resOutput :: !LazyText.Text
    , resCode   :: !Int
    }

runInterpreter :: forall f e. FrameEmulator f e
               => e
               -> IRData f
               -> Text
               -> IO (Either InterpreterError InterpreterResult)
runInterpreter emu IRData{..} input = do
    memory <- Memory.newMemory @(Word e) (Size 64) (Address memBaseAddr)
    stack <- Memory.newStack @(Word e) (Size 64) (Address stBaseAddr)
    regs <- Memory.newGrowVector @(Word e) (Size 64)
    outputRef <- newIORef mempty
    fpRef <- newIORef stBaseAddr
    stmtRef <- newIORef undefined
    currentFuncRef <- newIORef mainLabel
    inputPosRef <- newIORef 0

    stringsMap <- HashTable.initialize 16
    labelsMap <- HashTable.initialize 64
    let insertString LabeledString{..} = do
            Address addr <- Memory.allocMemory memory (Size (wordSize (Proxy @f)))
            liftIO $ HashTable.insert stringsMap addr lStringValue
            liftIO $ HashTable.insert labelsMap lStringLabel addr
    mapM_ insertString resStrings

    funcsMap <- liftIO $ HashTable.initialize 16
    let insertFunc IRFunction{..} = do
            Vec.imapM_ insertLabel body
            HashTable.insert funcsMap key func
          where key = frameName irFuncFrame
                func = Function { funExpr  = runInterpreterM body
                                , funFrame = irFuncFrame
                                }
                body = Vec.fromList irFuncBody
                insertLabel idx = \case
                    Label l -> HashTable.insert labelsMap l idx
                    _       -> pure ()
    liftIO $ mapM_ insertFunc resFunctions

    let ctx = Context { ctxMemory          = memory
                      , ctxStack           = stack
                      , ctxRegisters       = regs
                      , ctxOutput          = outputRef
                      , ctxInput           = input
                      , ctxInputPos        = inputPosRef
                      , ctxFramePointer    = fpRef
                      , ctxFunctions       = funcsMap
                      , ctxLabels          = labelsMap
                      , ctxStrings         = stringsMap
                      , ctxCurrentStmt     = stmtRef
                      , ctxCurrentFunction = currentFuncRef
                      , ctxEmulator        = emu
                      }

    let callMain = void $ callFunction mainLabel []
    let result code = do
            output <- readIORef outputRef
            pure $ Right $ InterpreterResult { resOutput = Builder.toLazyText output
                                             , resCode = code
                                             }
    handle errorHandler (runReaderT (Right <$> runInterpretM callMain) ctx) >>= \case
        Left (Exit code) -> result code
        Left err         -> pure $ Left err
        Right ()         -> result 0

  where stBaseAddr = 0x7ff8
        memBaseAddr = 0x8000
        mainLabel = Temp.LabelText "main"

        errorHandler :: InterpreterError -> IO (Either InterpreterError a)
        errorHandler = pure . Left

data NextStmt a
    = NextStmt !Int
    | Return !(Maybe a)

runInterpreterM :: forall m f e. (FrameEmulator f e, MonadInterpret m f e)
                => Vector (IR Stmt)
                -> m f e (Maybe (Word e))
runInterpreterM = runInterpreterM' 0
  where runInterpreterM' idx vec = do
            setCurrentStmt stmt
            next <- case stmt of
                Assign dst src -> do
                    val <- getOperandValue src
                    setRegister dst val
                    pure $ NextStmt $ idx + 1
                Binop dst op op1 op2 -> do
                    val1 <- getOperandValue op1
                    val2 <- getOperandValue op2
                    setRegister dst (binop op val1 val2)
                    pure $ NextStmt $ idx + 1
                Load dst src -> do
                    addr <- Address . fromIntegral <$> getOperandValue src
                    val <- readMemory addr
                    setRegister dst val
                    pure $ NextStmt $ idx + 1
                Store dst src -> do
                    addr <- Address . fromIntegral <$> getOperandValue dst
                    val <- getRegister src
                    writeMemory addr val
                    pure $ NextStmt $ idx + 1
                Label{} -> pure $ NextStmt $ idx + 1
                Jump l -> NextStmt <$> getLabelValue l
                CJump op op1 op2 tLab fLab -> do
                    val1 <- getOperandValue op1
                    val2 <- getOperandValue op2
                    let nextLab = if relop op val1 val2 then tLab else fLab
                    NextStmt <$> getLabelValue nextLab
                Call mDst funLabel args -> do
                    argVals <- mapM getOperandValue args
                    let regularCall = callFunction funLabel argVals
                    mRes <- case funLabel of
                        Temp.LabelText funName
                            | Just libFunc <- HashMap.lookup funName libFuncs
                            -> libFunc argVals
                            | otherwise
                            -> regularCall
                        _ -> regularCall
                    case (mDst, mRes) of
                        (Nothing, _)         -> pure ()
                        (Just{}, Nothing)    -> throwError $ VoidFunctionAssignment stmt
                        (Just dst, Just src) -> setRegister dst src
                    pure $ NextStmt $ idx + 1
                Ret val -> case val of
                    Nothing -> pure $ Return Nothing
                    Just t  -> Return . Just <$> getOperandValue t

            case next of
                NextStmt nextIdx
                    | nextIdx >= Vec.length vec -> do
                          funcLabel <- getCurrentFunc
                          throwError $ NoReturnAtFunctionEnd funcLabel
                    | otherwise -> runInterpreterM' nextIdx vec
                Return val -> pure val
          where stmt = vec Vec.! idx

                binop :: Binop -> Word e -> Word e -> Word e
                binop = \case
                    Add    -> (+)
                    Sub    -> (-)
                    Mul    -> (*)
                    Div    -> div
                    And    -> (.&.)
                    Or     -> (.|.)
                    Xor    -> xor
                    LShift -> \a b -> a `shiftL` (fromIntegral b)
                    RShift -> \a b -> a `shiftR` (fromIntegral b)

                relop :: Relop -> Word e -> Word e -> Bool
                relop = \case
                    Eq  -> (==)
                    Ne  -> (/=)
                    Lt  -> (<)
                    Le  -> (<=)
                    Gt  -> (>)
                    Ge  -> (>=)
                    ULt -> unsignedOp (<)
                    ULe -> unsignedOp (<=)
                    UGt -> unsignedOp (>)
                    UGe -> unsignedOp (>=)

                unsignedOp :: (UWord e -> UWord e -> Bool) -> Word e -> Word e -> Bool
                unsignedOp op a b = op (fromIntegral a) (fromIntegral b)

                getOperandValue :: IR Operand -> m f e (Word e)
                getOperandValue = \case
                    Const c -> pure $ fromIntegral c
                    Name l  -> fromIntegral <$> getLabelValue l
                    Temp t  -> getRegister t

type LibFunc m f e = [Word e] -> m f e (Maybe (Word e))

libFuncs :: forall m f e. (FrameEmulator f e, MonadInterpret m f e)
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
                    ]

        printFunc :: LibFunc m f e
        printFunc [str] = do
            getString (Address $ fromIntegral str) >>= printString
            pure Nothing
        printFunc args = argumentsNumMismatch 1 (length args)

        flushFunc :: LibFunc m f e
        flushFunc []   = pure Nothing
        flushFunc args = argumentsNumMismatch 0 (length args)

        getCharFunc :: LibFunc m f e
        getCharFunc [] = do
            str <- readChar
            Address addr <- newString str
            pure $ Just $ fromIntegral addr
        getCharFunc args = argumentsNumMismatch 0 (length args)

        ordFunc :: LibFunc m f e
        ordFunc [addr] = do
            str <- getString (Address $ fromIntegral addr)
            if Text.null str
            then pure $ Just (-1)
            else pure $ Just $ fromIntegral $ ord $ Text.head str
        ordFunc args = argumentsNumMismatch 1 (length args)

        chrFunc :: LibFunc m f e
        chrFunc [code] = do
            let char = chr (fromIntegral code)
            Address addr <- newString (Text.singleton char)
            pure $ Just $ fromIntegral addr
        chrFunc args = argumentsNumMismatch 1 (length args)

        sizeFunc :: LibFunc m f e
        sizeFunc [addr] = do
            str <- getString (Address $ fromIntegral addr)
            pure $ Just $ fromIntegral $ Text.length str
        sizeFunc args = argumentsNumMismatch 1 (length args)

        substringFunc :: LibFunc m f e
        substringFunc [addr, from, size] = do
            str <- getString (Address $ fromIntegral addr)
            let len = Text.length str
            let substr = if len > fromInt
                         then Text.take sizeInt $ Text.drop fromInt str
                         else ""
            Address substrAddr <- newString substr
            pure $ Just $ fromIntegral substrAddr
          where fromInt = fromIntegral from
                sizeInt = fromIntegral size
        substringFunc args = argumentsNumMismatch 3 (length args)

        concatFunc :: LibFunc m f e
        concatFunc [addr1, addr2] = do
            str1 <- getString (Address $ fromIntegral addr1)
            str2 <- getString (Address $ fromIntegral addr2)
            Address concatAddr <- newString $ str1 <> str2
            pure $ Just $ fromIntegral concatAddr
        concatFunc args = argumentsNumMismatch 2 (length args)

        notFunc :: LibFunc m f e
        notFunc [arg] = pure $ Just $ if arg == 0 then 1 else 0
        notFunc args  = argumentsNumMismatch 1 (length args)

        exitFunc :: LibFunc m f e
        exitFunc [code] = throwError $ Exit $ fromIntegral code
        exitFunc args   = argumentsNumMismatch 1 (length args)

        createArrayFunc :: LibFunc m f e
        createArrayFunc [size, init] = do
            Address addr <- allocateMemory (Size $ fromIntegral size * ws)
            forM_ [addr, addr + ws .. addr + sizeInt] $ \curAddr ->
                writeMemory (Address curAddr) init
            pure $ Just $ fromIntegral addr
          where sizeInt = fromIntegral size
                ws= wordSize (Proxy @f)
        createArrayFunc args = argumentsNumMismatch 2 (length args)

        stringEqualFunc :: LibFunc m f e
        stringEqualFunc [addr1, addr2] = do
            str1 <- getString (Address $ fromIntegral addr1)
            str2 <- getString (Address $ fromIntegral addr2)
            pure $ Just $ if str1 == str2 then 1 else 0
        stringEqualFunc args = argumentsNumMismatch 2 (length args)

        argumentsNumMismatch :: Int -> Int -> m f e a
        argumentsNumMismatch exp act = do
            stmt <- getCurrentStmt
            throwError $ ArgumentsNumberMismatch exp act stmt

isAddrInStack :: Address -> InterpretM f e Bool
isAddrInStack addr = do
    stack <- asks ctxStack
    pure $ stackBaseAddr stack >= addr

getExceptionHandler :: (IR Stmt -> InterpreterError)
                    -> InterpretM f e (ErrorCall -> IO b)
getExceptionHandler cons = do
    stmtRef <- asks ctxCurrentStmt
    pure $ hdl stmtRef
  where hdl stmtRef _ = readIORef stmtRef >>= throwIO . cons

throwInterpretException :: (IR Stmt -> InterpreterError) -> InterpretM f e b
throwInterpretException cons = do
    stmtRef <- asks ctxCurrentStmt
    liftIO (readIORef stmtRef >>= throwIO . cons)
