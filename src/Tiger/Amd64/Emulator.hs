module Tiger.Amd64.Emulator
    ( IREmulator(..)
    , TempRegEmulator(..)
    ) where

import           Control.Monad           (forM_)
import           Data.Int                (Int64)
import           Data.Proxy              (Proxy (..))
import           Data.Word               (Word64)

import           Tiger.Amd64.Assem.Types (CallingConvention (..), Instr, Reg (..))
import           Tiger.Amd64.Frame       (Frame (..))
import           Tiger.Codegen           (TempReg (..))
import           Tiger.Frame             (Access (..), wordSize)
import           Tiger.IR.Types          (Stmt)
import           Tiger.RegMachine        (Address (..), MonadInterpret (..), Size (..))
import           Tiger.Temp              (Temp (..))

import qualified Tiger.RegMachine        as RegMachine

data IREmulator = IREmulator

instance CallingConvention f => RegMachine.Emulator IREmulator f Temp Stmt where
    type Word IREmulator = Int64
    type UWord IREmulator = Word64

    newEmulator = const $ pure IREmulator
    enterFunction IREmulator frame args = do
        let (allocSize, Size varsOffset) = getIRAllocSize frame
        sp <- allocateStack allocSize
        let nextFP = getAddress sp + varsOffset
        curFP <- getRegister FP
        setRegister FP (fromIntegral nextFP)
        writeMemory (Address nextFP) curFP
        forM_ (zip frArgs args) $ \case
            (InReg t, val)     -> setRegister t val
            (InFrame off, val) -> writeMemory (Address $ nextFP + off) val
      where Frame{..} = toAmd64Frame frame
    exitFunction IREmulator frame = do
        curFP <- fromIntegral <$> getRegister FP
        oldFP <- readMemory (Address curFP)
        setRegister FP oldFP
        let deallocSize = fst $ getIRAllocSize frame
        reduceStack deallocSize

getIRAllocSize :: forall f. CallingConvention f => f -> (Size, Size)
getIRAllocSize frame = ( Size $ frArgsOffset + frCurOffset + wordSize (Proxy @f)
                       , Size frCurOffset
                       )
  where Frame{..} = toAmd64Frame frame

data TempRegEmulator = TempRegEmulator

instance CallingConvention f =>
    RegMachine.Emulator TempRegEmulator f (TempReg Reg) (Instr (TempReg Reg)) where
    type Word TempRegEmulator = Int64
    type UWord TempRegEmulator = Word64

    newEmulator = const $ pure TempRegEmulator

    enterFunction TempRegEmulator frame _ = do
        let (allocSize, rbpOffset) = getTempRegAllocSize frame
        fp <- getRegister (Reg Rbp)
        sp <- allocateStack allocSize
        let rbpAddr = Address $ getAddress sp + rbpOffset
        writeMemory rbpAddr fp
        setRegister (Reg Rbp) (fromIntegral $ getAddress rbpAddr)
        setRegister (Reg Rsp) (fromIntegral $ getAddress sp)

    exitFunction TempRegEmulator frame = do
        let (Size allocSize, _) = getTempRegAllocSize frame
        sp <- getRegister (Reg Rsp)
        fp <- getRegister (Reg Rbp)
        oldFp <- readMemory (Address $ fromIntegral fp)
        setRegister (Reg Rbp) oldFp
        reduceStack (Size allocSize)
        setRegister (Reg Rsp) (sp + fromIntegral allocSize)

getTempRegAllocSize :: forall f. CallingConvention f => f -> (Size, Int)
getTempRegAllocSize frame = (Size $ frCurOffset + 2 * wordSize (Proxy @f), frCurOffset)
  where Frame{..} = toAmd64Frame frame
