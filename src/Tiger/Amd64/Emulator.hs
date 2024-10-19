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
import           Tiger.RegMachine        (MonadInterpret (..))
import           Tiger.RegMachine.Memory (Address (..), Size (..))
import           Tiger.Temp              (Temp (..))

import qualified Tiger.RegMachine        as RegMachine

data IREmulator = IREmulator

instance CallingConvention f => RegMachine.Emulator IREmulator f Temp Stmt where
    type Word IREmulator = Int64
    type UWord IREmulator = Word64

    newEmulator = const $ pure IREmulator
    enterFunction IREmulator frame args = do
        let (allocSize, Size varsOffset) = getAllocSize frame
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
        let deallocSize = fst $ getAllocSize frame
        reduceStack deallocSize

data TempRegEmulator = TempRegEmulator

instance CallingConvention f =>
    RegMachine.Emulator TempRegEmulator f (TempReg Reg) (Instr (TempReg Reg)) where
    type Word TempRegEmulator = Int64
    type UWord TempRegEmulator = Word64

    newEmulator = const $ pure TempRegEmulator

    enterFunction TempRegEmulator _ _ = do
        fp <- getRegister (Reg Rbp)
        sp <- allocateStack (Size (wordSize (Proxy @f)))
        writeMemory sp fp
        setRegister (Reg Rbp) (fromIntegral $ getAddress sp)
        setRegister (Reg Rsp) (fromIntegral $ getAddress sp)

    exitFunction TempRegEmulator _ = do
        fp <- getRegister (Reg Rbp)
        oldFp <- readMemory (Address $ fromIntegral fp)
        setRegister (Reg Rbp) oldFp
        reduceStack (Size $ wordSize (Proxy @f))
        setRegister (Reg Rsp) (fp + fromIntegral (wordSize $ Proxy @f))

getAllocSize :: forall f. CallingConvention f => f -> (Size, Size)
getAllocSize frame = ( Size $ frArgsOffset + frCurOffset + wordSize (Proxy @f)
                     , Size frCurOffset
                     )
  where Frame{..} = toAmd64Frame frame
