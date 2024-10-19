module Tiger.Frame
    ( Access(..)
    , Frame(..)
    ) where

import           Data.Kind                  (Type)
import           Data.Proxy                 (Proxy)
import           Data.Text                  (Text)
import           Data.Text.Lazy.Builder.Int (decimal)

import           Tiger.Codegen.Assem        (Instruction, TempReg)
import           Tiger.DList                (DList)
import           Tiger.Expr                 (Escaping)
import           Tiger.IR.Types             (Expr, Stmt)
import           Tiger.Temp                 (Label, MonadTemp (..), Temp)
import           Tiger.TextUtils            (TextBuildable (..))

data Access
    = InReg !Temp
    | InFrame !Int

instance TextBuildable Access where
    toTextBuilder = \case
        InReg t     -> toTextBuilder t
        InFrame off -> "FP[" <> decimal off <> "]"

class TextBuildable a => Frame a where
    type Reg a :: Type
    type Instr a :: Type -> Type

    newFrame :: MonadTemp m => Label -> [Escaping] -> m a
    frameName :: a -> Label
    frameArgs :: a -> [Access]
    allocLocal :: MonadTemp m => a -> Escaping -> m (a, Access)
    wordSize :: Proxy a -> Int
    accessToIR :: MonadTemp m => a -> Access -> Expr -> m Expr
    externalCall :: Proxy a -> Text -> [Expr] -> Expr
    procEntryExit1 :: a -> Stmt -> Stmt
    codegen :: ( MonadTemp m
               , Instruction (Instr a) (TempReg (Reg a))
               , Instruction (Instr a) (Reg a)
               )
            => a
            -> Stmt
            -> m (DList (Instr a (TempReg (Reg a))))
