module Tiger.Amd64.Assem.Interpreter where

import           Control.Monad             (forM_)
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Reader      (MonadReader (..), ReaderT (..), asks)
import           Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           Data.Typeable             (Typeable)
import           Data.Vector.Hashtables    (PrimMonad)

import           Tiger.Amd64.Assem.Types   (CallingConvention (..), Instr (..), Reg (..))
import           Tiger.Amd64.Emulator      (TempRegEmulator)
import           Tiger.Codegen             (TempReg (..))
import           Tiger.Frame               (frameName)
import           Tiger.IR.Types            (Block (..), ControlFlowGraph (..),
                                            IRData (..), IRFunction (..))
import           Tiger.RegMachine          (Emulator, FrameEmulator, FrameRegister (..),
                                            Function (..), HashTable, InterpretM,
                                            Interpretable (..), InterpreterError,
                                            InterpreterResult (..), LabelValue (..),
                                            MonadInterpret (..), ReturnRegister (..),
                                            Trans (..), initFunctionsCFG)
import           Tiger.TextUtils           (TextBuildable (..))

newtype Amd64InterpretM f e r s a = Amd64InterpretM
    { runAmd64InterpretM :: ReaderT (IORef Bool) (InterpretM f e r s) a
    } deriving newtype ( Functor
                       , Applicative
                       , Monad
                       , MonadIO
                       , MonadError (InterpreterError r s)
                       , MonadReader (IORef Bool)
                       )

deriving via (Trans (ReaderT (IORef Bool)) InterpretM)
    instance (Typeable r, TextBuildable r, Enum r, Typeable s, TextBuildable s) =>
        MonadInterpret f e r s Amd64InterpretM

class (CallingConvention f, MonadInterpret f e r s m) =>
    Amd64MonadInterpret f e r s m where
    getCondFlag :: m f e r s Bool
    setCondFlag :: Bool -> m f e r s ()

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
    initFunctions = initFunctionsCFG runInstrM getUnlift
        where getUnlift :: IO (Amd64InterpretM f e r s () -> InterpretM f e r s ())
              getUnlift = do
                  condRef <- newIORef False
                  pure (\ma -> runReaderT (runAmd64InterpretM ma) condRef)

type InstrInterpreter m f e r s = ( FrameEmulator f e r s
                                  , MonadError (InterpreterError r s) (m f e r s)
                                  , Amd64MonadInterpret f e r s m
                                  )


runInstrM :: InstrInterpreter m f e r (Instr r) => Instr r -> m f e r (Instr r) ()
runInstrM = undefined

{-

instance ( MonadIO m
         , PrimMonad m
         , CallingConvention f
         , Emulator TempRegEmulator f (TempReg Reg) (Instr (TempReg Reg))
         )
  => Interpretable f TempRegEmulator
    (TempReg Reg) (Instr (TempReg Reg))
    (ControlFlowGraph (Instr (TempReg Reg))) (ReaderT (IORef Bool) m) where
    initFunctions IRData{..} funcsMap labelsMap = mapM_ insertFunc irFunctions
      where insertFunc IRFunction{..} = do
                let key = frameName irFuncFrame
                let (_, _, startBlock, _) = fromJust
                                          $ fst
                                          $ Graph.match 0
                                          $ cfgGraph irFuncBody
                let func = Function { funExpr  = runAmd64InterpretM $ runInterpreterM startBlock
                                    , funFrame = irFuncFrame
                                    }
                HashTable.insert funcsMap key func
                forM_ (Graph.labNodes $ cfgGraph irFuncBody) $ \(_, block@Block{..}) -> do
                    let labelVal = LabelPos $ runAmd64InterpretM $ runInterpreterM block
                    HashTable.insert labelsMap blockLabel labelVal

runInterpreter :: forall m f e r b. ( PrimMonad m
                                    , MonadIO m
                                    , CallingConvention f
                                    , Enum r
                                    , FrameEmulator f e r (Instr r)
                                    , Interpretable f e r (Instr r) b (ReaderT (IORef Bool) m)
                                    )
               => e
               -> ReturnRegister r
               -> FrameRegister r
               -> IRData b f
               -> Text
               -> m (InterpreterResult r (Instr r))
runInterpreter emu rv fp ir@IRData{..} input = do
    condRef <- liftIO $ newIORef False
    funcsMap <- HashTable.initialize 16
    labelsMap <- HashTable.initialize 64
    initFunctions ir funcsMap labelsMap
    let intA = runAmd64InterpretM callMain
    let rdrA = runInterpretT rv fp emu funcsMap labelsMap irStrings input intA
    runReaderT rdrA condRef

runInterpreterM :: InstrInterpreter m f e r => Block (Instr r) -> m f e r (Instr r) ()
runInterpreterM = undefined

runInstrM :: InstrInterpreter m f e r => Instr r -> m f e r (Instr r) ()
runInstrM = undefined
-}
