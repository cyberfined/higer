module Tiger.Frame
    ( Access(..)
    , Frame(..)
    , accessBuilder
    ) where

import           Data.Proxy                 (Proxy)
import           Data.Text                  (Text)
import           Data.Text.Lazy.Builder     (Builder)
import           Data.Text.Lazy.Builder.Int (decimal)

import           Tiger.Expr                 (Escaping)
import           Tiger.IR.Types             (Expr, Stmt)
import           Tiger.Temp                 (Label, MonadTemp (..), Temp, tempBuilder)

data Access
    = InReg !Temp
    | InFrame !Int

accessBuilder :: Access -> Builder
accessBuilder = \case
    InReg t     -> tempBuilder t
    InFrame off -> "FP[" <> decimal off <> "]"

class Frame a where
    newFrame :: MonadTemp m => Label -> [Escaping] -> m a
    frameName :: a -> Label
    frameArgs :: a -> [Access]
    allocLocal :: MonadTemp m => a -> Escaping -> m (a, Access)
    wordSize :: Proxy a -> Int
    accessToIR :: MonadTemp m => a -> Access -> Expr -> m Expr
    externalCall :: Proxy a -> Text -> [Expr] -> Expr
    procEntryExit1 :: a -> Stmt -> Stmt
    frameBuilder :: a -> Builder
