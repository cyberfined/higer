module Tiger.Temp
    ( Temp(..)
    , tempBuilder
    , Label(..)
    , labelBuilder
    , MonadTemp(..)
    ) where

import           Data.Hashable              (Hashable (..))
import           Data.Text                  (Text)
import           Data.Text.Lazy.Builder     (Builder, fromText, singleton)
import           Data.Text.Lazy.Builder.Int (decimal)

data Temp = Temp !Int
          | FP
          deriving Eq

tempBuilder :: Temp -> Builder
tempBuilder (Temp t) = singleton 't' <> decimal t
tempBuilder FP       = "FP"

data Label = LabelInt !Int
           | LabelText !Text
           deriving Eq

instance Hashable Label where
    hashWithSalt salt = \case
        LabelInt i  -> hashWithSalt salt i
        LabelText s -> hashWithSalt salt s

labelBuilder :: Label -> Builder
labelBuilder (LabelInt l)  = singleton 'l' <> decimal l
labelBuilder (LabelText l) = fromText l

class Monad m => MonadTemp m where
    newTemp  :: m Temp
    newLabel :: m Label
