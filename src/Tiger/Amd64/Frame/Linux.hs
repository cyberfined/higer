module Tiger.Amd64.Frame.Linux (LinuxFrame) where

import           Data.Bifunctor      (first)

import           Tiger.Amd64.Assem   (CallingConvention (..), Instr, Reg (..))
import           Tiger.Amd64.Codegen (codegen, gpRegisters)
import           Tiger.IR.Types      (Expr (..))
import           Tiger.Temp          hiding (Temp)
import           Tiger.TextUtils     (TextBuildable (..))

import qualified Tiger.Amd64.Frame   as Amd64
import qualified Tiger.Frame         as FrameClass

newtype LinuxFrame = LinuxFrame { getFrame :: Amd64.Frame }

instance TextBuildable LinuxFrame where
    toTextBuilder = Amd64.frameBuilder "Amd64.LinuxFrame" . getFrame

instance FrameClass.Frame LinuxFrame where
    type Reg LinuxFrame = Reg
    type Instr LinuxFrame = Instr

    newFrame label args = LinuxFrame <$> Amd64.newFrame 6 label args
    frameName = Amd64.frameName . getFrame
    frameArgs = Amd64.frameArgs . getFrame
    allocLocal fr esc = first LinuxFrame <$> Amd64.allocLocal (getFrame fr) esc
    wordSize = const Amd64.wordSize
    accessToIR frame access frameAddress =
        Amd64.accessToIR (getFrame frame) access frameAddress
    externalCall _ name args = Call (LabelText name) args
    procEntryExit1 (LinuxFrame frame) = Amd64.procEntryExit1 frame
    codegen = codegen
    gpRegisters _ = gpRegisters

instance CallingConvention LinuxFrame where
    argsRegisters _       = [Rdi, Rsi, Rdx, Rcx, R8, R9]
    calleeSaveRegisters _ = [Rbx, R12, R13, R14, R15]
    toAmd64Frame          = getFrame
