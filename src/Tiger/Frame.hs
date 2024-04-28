module Tiger.Frame
    ( Frame(..)
    , Access(..)
    , accessBuilder
    ) where

import           Control.Monad.IO.Class     (MonadIO)
import           Data.Proxy                 (Proxy)
import           Data.Text.Lazy.Builder     (Builder, singleton)
import           Data.Text.Lazy.Builder.Int (decimal)

import           Tiger.Expr                 (Escaping (..))
import           Tiger.IR.Types             (ExternalFun, IR, Operand, Stmt)
import           Tiger.Temp                 (Label, MonadTemp, Temp, tempBuilder)

data Access
    = InReg !Temp
    | InFrame !Int

accessBuilder :: Access -> Builder
accessBuilder = \case
    InReg t     -> tempBuilder t
    InFrame off -> "FP[" <> decimal off <> singleton ']'

class Frame a where
    newFrame :: (MonadIO m, MonadTemp m) => Label -> [Escaping] -> m a
    frameName :: a -> Label
    frameArgs :: a -> [Access]
    allocLocal :: (MonadIO m, MonadTemp m) => a -> Escaping -> m (Access)
    wordSize :: Proxy a -> Int
    accessToIR :: MonadTemp m => a -> Access -> IR Operand -> m ([IR Stmt], Temp)
    procEntryExit :: a -> ([IR Stmt], [IR Stmt])
    externalCall :: Proxy a -> Maybe Temp -> ExternalFun -> [IR Operand] -> IR Stmt
