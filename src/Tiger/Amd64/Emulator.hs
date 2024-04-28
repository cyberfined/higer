module Tiger.Amd64.Emulator
    ( Emulator
    , createEmulator
    ) where

import           Control.Monad               (forM_)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Int                    (Int64)
import           Data.IORef                  (readIORef)
import           Data.Proxy                  (Proxy(..))
import           Data.Word                   (Word64)

import           Tiger.Amd64.Frame
import           Tiger.Frame                 (Access (..), wordSize)
import           Tiger.IR.Interpreter        (MonadInterpret (..))
import           Tiger.IR.Interpreter.Memory (Address (..), Size (..))
import           Tiger.Temp                  (Temp (..))

import qualified Tiger.IR.Interpreter        as EmulatorClass

data Emulator = Emulator

createEmulator :: Emulator
createEmulator = Emulator

instance EmulatorClass.Emulator Emulator Frame where
    type Word Emulator = Int64
    type UWord Emulator = Word64

    enterFunction Emulator frame@Frame{..} args = do
        (allocSize, Size varsOffset) <- getAllocSize frame
        sp <- allocateStack allocSize
        let nextFP = getAddress sp + varsOffset
        curFP <- getRegister FP
        setRegister FP (fromIntegral nextFP)
        writeMemory (Address nextFP) curFP
        forM_ (zip frArgs args) $ \case
            (InReg t, val)     -> setRegister t val
            (InFrame off, val) -> writeMemory (Address $ nextFP + off) val
    exitFunction Emulator frame = do
        curFP <- fromIntegral <$> getRegister FP
        oldFP <- readMemory (Address curFP)
        setRegister FP oldFP
        (deallocSize, _) <- getAllocSize frame
        reduceStack deallocSize
        
getAllocSize :: MonadIO m => Frame -> m (Size, Size)
getAllocSize Frame{..} = liftIO $ do
    argsOffset <- readIORef frArgsOffset
    varsOffset <- readIORef frCurOffset
    pure (Size $ argsOffset + varsOffset + wordSize (Proxy @Frame), Size varsOffset)
