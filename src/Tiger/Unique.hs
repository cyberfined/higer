module Tiger.Unique
    ( Unique(..)
    , MonadUnique(..)
    ) where

newtype Unique = Unique Int deriving Eq

class Monad m => MonadUnique m where
    newUnique :: m Unique
