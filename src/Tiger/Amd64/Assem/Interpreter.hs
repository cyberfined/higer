{-# LANGUAGE DataKinds #-}

module Tiger.Amd64.Assem.Interpreter () where

import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Reader      (MonadReader (..), ReaderT (..))
import           Data.Bits                 (shiftL, shiftR, xor, (.&.))
import           Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import           Data.Proxy                (Proxy (..))
import           Data.Typeable             (Typeable)
import           Prelude                   hiding (Word, quot, rem)

import           Tiger.Amd64.Assem.Types   (Base (..), CallingConvention (..),
                                            Condition (..), Imul (..), Index (..),
                                            Instr (..), InstrOperands, Offset (..),
                                            Operand (..), OperandType (..), Reg (..),
                                            RegMem, Scale (..))
import           Tiger.Codegen             (TempReg (..))
import           Tiger.Frame               (wordSize)
import           Tiger.IR.Types            (ControlFlowGraph (..))
import           Tiger.RegMachine          (Address (..), Emulator (..), FrameEmulator,
                                            InterpretM, Interpretable (..),
                                            InterpreterError (..), MonadInterpret (..),
                                            Size (..), Trans (..), initFunctionsCFG,
                                            libFuncs, withCurrentStmt)
import           Tiger.TextUtils           (TextBuildable (..))

import qualified Data.HashMap.Strict       as HashMap

import qualified Tiger.Temp                as Temp

newtype Amd64InterpretM f e r s a = Amd64InterpretM
    { runAmd64InterpretM :: ReaderT (IORef Condition) (InterpretM f e r s) a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadIO
                       , MonadError (InterpreterError r s)
                       , MonadReader (IORef Condition)
                       )

deriving via (Trans (ReaderT (IORef Condition)) InterpretM)
    instance (Typeable r, TextBuildable r, Enum r, Typeable s, TextBuildable s) =>
        MonadInterpret f e r s Amd64InterpretM

class (CallingConvention f, MonadInterpret f e r s m) =>
    Amd64MonadInterpret f e r s m where
    getCondFlag :: m f e r s Condition
    setCondFlag :: Condition -> m f e r s ()

instance ( CallingConvention f
         , Typeable r
         , TextBuildable r
         , Enum r
         , Typeable s
         , TextBuildable s
         ) => Amd64MonadInterpret f e r s Amd64InterpretM where
    getCondFlag = ask >>= liftIO . readIORef
    setCondFlag val = ask >>= \condRef -> liftIO $ writeIORef condRef val

instance (CallingConvention f, Emulator e f (TempReg Reg) (Instr (TempReg Reg))) =>
    Interpretable f e (TempReg Reg) (Instr (TempReg Reg))
        (ControlFlowGraph (Instr (TempReg Reg))) where
    initFunctions = initFunctionsCFG (runInstrM Reg) getUnlift
        where getUnlift :: IO (Amd64InterpretM f e r s () -> InterpretM f e r s ())
              getUnlift = do
                  condRef <- newIORef Eq
                  pure (\ma -> runReaderT (runAmd64InterpretM ma) condRef)

type InstrInterpreter m f e r s = ( FrameEmulator f e r s
                                  , MonadError (InterpreterError r s) (m f e r s)
                                  , Amd64MonadInterpret f e r s m
                                  )


runInstrM :: forall m f e r. InstrInterpreter m f e r (Instr r)
          => (Reg -> r)
          -> Instr r
          -> m f e r (Instr r) ()
runInstrM fromReg instr = withCurrentStmt instr $ case instr of
    Add dst src -> runBinop (+) dst src
    Sub dst src -> runBinop (-) dst src
    Xor dst src -> runBinop xor dst src
    Lea (Register dst) src -> getAddress src >>= setRegister dst
    Sal dst (Const n) -> runUnOp (`shiftL` fromIntegral n) dst
    Sar dst (Const n) -> runUnOp (`shiftR` fromIntegral n) dst
    Imul imul -> case imul of
        Imul2 dst src -> runBinop (*) dst src
        Imul3 (Register dst) src (Const n) -> do
            srcVal <- getOperandValue src
            setRegister dst (srcVal * fromIntegral n)
    Idiv (Register src) -> do
        -- TODO: emulate rdx:rax / src division
        {-
        rdx <- getRegister (fromReg Rdx)
        rax <- getRegister (fromReg Rax)
        let val1 = (fromIntegral @(Word e) @Integer rdx `shiftL` 64) .|. fromIntegral rax
        val2 <- fromIntegral <$> getRegister src
        let (quot, rem) = val1 `divMod` val2
        setRegister (fromReg Rax) (fromIntegral quot)
        setRegister (fromReg Rdx) (fromIntegral rem)
        -}
        val1 <- getRegister (fromReg Rax)
        val2 <- getRegister src
        setRegister (fromReg Rax) (val1 `div` val2)
    Cqo -> do
        rax <- getRegister (fromReg Rax)
        let rdxValue = if rax < 0 then -maxBound - 1 else 0
        setRegister (fromReg Rdx) rdxValue
    Mov dst src -> do
        srcVal <- getOperandValue src
        case dst of
            Register r -> setRegister r srcVal
            AddrRegBase{} -> getAddress dst
                          >>= (`writeMemory` srcVal) . Address . fromIntegral
            AddrRegBaseIndex{} -> getAddress dst
                               >>= (`writeMemory` srcVal) . Address . fromIntegral
    Test op1 op2 -> runCmpOp (\a b -> (a .&. b, 0)) op1 op2
    Cmp op1 op2 -> runCmpOp (,) op1 op2
    Jmp l -> jumpLabel l
    Jcc cond tLab fLab -> do
        flag <- getCondFlag
        let shouldJump = case (cond, flag) of
                (Gt, Gt) -> True
                (Ge, Gt) -> True
                (Ge, Eq) -> True
                (Eq, Eq) -> True
                (Ne, Eq) -> False
                (Ne, _)  -> True
                (Lt, Lt) -> True
                (Le, Lt) -> True
                (Le, Eq) -> True
                _        -> False
        if shouldJump then jumpLabel tLab else jumpLabel fLab
    Ret -> throwError EndOfFunction
    Label{} -> pure ()
    Call funLabel args -> case funLabel of
        Temp.LabelText funName
          | Just libFunc <- HashMap.lookup funName libFuncs
          -> mapM getOperandValue args >>= libFunc
        _ -> callFunction funLabel []
    Push src -> do
        val <- getOperandValue src
        Address sp <- allocateStack (Size $ wordSize (Proxy @f))
        writeMemory (Address $ fromIntegral sp) val
        setRegister (fromReg Rsp) (fromIntegral sp)
    Neg dst -> runUnOp negate dst
  where runBinop :: InstrOperands t1 t2
                 => (Word e -> Word e -> Word e)
                 -> Operand r t1
                 -> Operand r t2
                 -> m f e r (Instr r) ()
        runBinop f dst src = case dst of
            Register r -> do
                val <- f <$> getRegister r <*> getOperandValue src
                setRegister r val
            AddrRegBase{} -> do
                addr <- Address . fromIntegral <$> getAddress dst
                val <- f <$> getOperandValue dst <*> getOperandValue src
                writeMemory addr val
            AddrRegBaseIndex{} -> do
                addr <- Address . fromIntegral <$> getAddress dst
                val <- f <$> getOperandValue dst <*> getOperandValue src
                writeMemory addr val

        runUnOp :: RegMem t => (Word e -> Word e) -> Operand r t -> m f e r (Instr r) ()
        runUnOp f = \case
            Register r -> getRegister r >>= setRegister r . f
            op@AddrRegBase{} -> do
                addr <- Address . fromIntegral <$> getAddress op
                val <- f <$> readMemory addr
                writeMemory addr val
            op@AddrRegBaseIndex{} -> do
                addr <- Address . fromIntegral <$> getAddress op
                val <- f <$> readMemory addr
                writeMemory addr val

        runCmpOp :: (Word e -> Word e -> (Word e, Word e))
                 -> Operand r t1
                 -> Operand r t2
                 -> m f e r (Instr r) ()
        runCmpOp f op1 op2 = do
            (a, b) <- f <$> getOperandValue op1 <*> getOperandValue op2
            let cond = case compare a b of
                    LT -> Lt
                    EQ -> Eq
                    GT -> Gt
            setCondFlag cond

        getOperandValue :: Operand r t -> m f e r (Instr r) (Word e)
        getOperandValue = \case
            Register r            -> getRegister r
            op@AddrRegBase{}      -> getAddress op >>= readMemory . Address . fromIntegral
            op@AddrRegBaseIndex{} -> getAddress op >>= readMemory . Address . fromIntegral
            Const n               -> pure $ fromIntegral n
            Name l                -> fromIntegral <$> getLabelValue l

        getAddress :: Operand r 'OpMem -> m f e r (Instr r) (Word e)
        getAddress = \case
            AddrRegBase (Offset off) (Base base) -> do
                baseVal <- getRegister base
                pure $ fromIntegral off + baseVal
            AddrRegBaseIndex (Offset off) base (Index idx) (Scale scale) -> do
                baseVal <- maybe (pure 0) (getRegister . getBase) base
                idxVal <- getRegister idx
                pure $ fromIntegral off + baseVal + idxVal * fromIntegral scale
